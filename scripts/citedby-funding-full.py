# /// script
# dependencies = [
#   "aiohttp",
#   "pandas",
#   "tqdm",
# ]
# ///
"""
Funding Extraction Pipeline
===========================

Input TSV columns (minimum one identifier required):
    title
    pmid
    pmcid
    doi
    scopus_eid

Output TSV:
    pmid
    pmcid
    doi
    scopus_eid
    title
    agency
    subagency
    grant_id
    confidence
    sources
    evidence_text

Each input publication appears at least once. If no funding is identified, one row
is emitted with the funding fields empty. Its sources field lists sources that
matched the publication, or remains empty when no source returned a match.
Missing input values are also empty.

Usage:
    python citedby-funding-full.py \
        --input publications.tsv \
        --output funding_results.tsv

Dependencies:
    pip install pandas aiohttp tqdm
"""

import argparse
import asyncio
import csv
import hashlib
import json
import logging
import os
import re
import shutil
import sqlite3
import subprocess
import time
import xml.etree.ElementTree as ET
from collections import Counter
from pathlib import Path
from urllib.parse import quote, urlencode, urlsplit

import pandas as pd

try:
    import aiohttp
except ImportError:
    aiohttp = None

try:
    from tqdm.asyncio import tqdm_asyncio
except ImportError:
    tqdm_asyncio = None


###############################################################################
# CONFIG
###############################################################################

USER_AGENT = "FundingExtractor/1.0 (academic use; allenbaron@som.umaryland.edu)"

HTTP_TIMEOUT = 30
MAX_CONCURRENT_REQUESTS = 5
MAX_RETRIES = 3
CACHE_TTL_SECONDS = 60 * 60 * 24 * 30
NCBI_MIN_INTERVAL_SECONDS = 0.11
DEFAULT_CACHE_NAME = "funding_cache.sqlite3"

CREDENTIAL_NAME = "ENTREZ_KEY"
EXPECTED_OUTPUT_COLUMNS = [
    "pmid",
    "pmcid",
    "doi",
    "scopus_eid",
    "title",
    "agency",
    "subagency",
    "grant_id",
    "confidence",
    "sources",
    "evidence_text",
]
logger = logging.getLogger(__name__)


def read_input_tsv(path):
    """Read a quoted TSV while preserving embedded tabs and newlines."""
    with path.open("r", encoding="utf-8-sig", newline="") as handle:
        reader = csv.DictReader(handle, delimiter="\t", strict=True)
        if not reader.fieldnames or not reader.fieldnames[0]:
            raise ValueError("input TSV has no header")
        if len(reader.fieldnames) != len(set(reader.fieldnames)):
            raise ValueError("input TSV contains duplicate column names")

        rows = []
        for row in reader:
            if None in row:
                raise ValueError(
                    f"record ending on line {reader.line_num} has too many fields"
                )
            if any(value is None for value in row.values()):
                raise ValueError(
                    f"record ending on line {reader.line_num} has too few fields"
                )
            rows.append(row)

    return pd.DataFrame(rows, columns=reader.fieldnames, dtype=str)


def get_ncbi_api_key():
    """Read the NCBI key from a credential store without exposing it."""
    key = os.environ.get("ENTREZ_KEY", "").strip()
    if key:
        return key

    security = shutil.which("security")
    if security and os.uname().sysname == "Darwin":
        result = subprocess.run(
            [security, "find-generic-password", "-s", CREDENTIAL_NAME, "-w"],
            capture_output=True,
            text=True,
            check=False,
        )
        if result.returncode == 0:
            key = result.stdout.strip()
            if key:
                return key

    return None


###############################################################################
# NIH MAPPING
###############################################################################

NIH_IC_MAP = {
    "AA": "NIAAA",
    "AG": "NIA",
    "AI": "NIAID",
    "AR": "NIAMS",
    "AT": "NCCIH",
    "CA": "NCI",
    "DA": "NIDA",
    "DC": "NIDCD",
    "DE": "NIDCR",
    "DK": "NIDDK",
    "EB": "NIBIB",
    "ES": "NIEHS",
    "EY": "NEI",
    "GM": "NIGMS",
    "HD": "NICHD",
    "HG": "NHGRI",
    "HL": "NHLBI",
    "LM": "NLM",
    "MD": "NIMHD",
    "MH": "NIMH",
    "NR": "NINR",
    "NS": "NINDS",
    "OD": "OD",
    "RR": "NCRR",
    "TR": "NCATS",
    "TW": "FIC",
}

NIH_ACTIVITY_CODES = (
    "R01|R03|R15|R21|R25|R33|R34|R35|R37|R41|R42|R43|R44|R56|RM1|"
    "P01|P20|P30|P50|P60|U01|U19|U24|U54|K01|K08|K23|F30|F31|F32|"
    "T32|DP1|DP2|DP5|UL1|KL2|TL1|S10|ZIA|ZIC|UG3|UH2|UH3|UM1|UC6|UC7"
)

###############################################################################
# FUNDER NORMALIZATION
###############################################################################

FUNDER_NORMALIZATION = {
    "national institutes of health": ("NIH", None),
    "national cancer institute": ("NIH", "NCI"),
    "national institute of allergy and infectious diseases": ("NIH", "NIAID"),
    "national human genome research institute": ("NIH", "NHGRI"),
    "national institute of general medical sciences": ("NIH", "NIGMS"),
    "national institute of mental health": ("NIH", "NIMH"),
    "national heart lung and blood institute": ("NIH", "NHLBI"),
    "medical research council": ("UKRI", "MRC"),
    "biotechnology and biological sciences research council": ("UKRI", "BBSRC"),
    "engineering and physical sciences research council": ("UKRI", "EPSRC"),
    "economic and social research council": ("UKRI", "ESRC"),
    "wellcome trust": ("Wellcome Trust", None),
    "canadian institutes of health research": ("CIHR", None),
    "natural sciences and engineering research council": ("NSERC", None),
    "national science foundation": ("NSF", None),
    "national science foundation nsf": ("NSF", None),
}

###############################################################################
# GRANT REGEXESf
###############################################################################

NIH_GRANT_REGEXES = [
    re.compile(
        rf"\b[0-9]?\s*(?:{NIH_ACTIVITY_CODES})"
        r"\s*[A-Z]{2}\s*[0-9]{5,6}"
        r"(?:\s*-[0-9]{2}[A-Z0-9]*)?\b"
    ),
    re.compile(r"\b[0-9]{1,2}\s*[A-Z]{2}\s*[0-9]{5,6}\b"),
]

UKRI_REGEX = re.compile(r"\b[A-Z]{2}/[A-Z0-9]{5,10}/[0-9]\b")
CONTRACT_GRANT_REGEX = re.compile(r"\b(?:HHSN[0-9A-Z-]+|N0[0-9][A-Z]{2}[0-9A-Z]+)\b")
ALPHANUMERIC_GRANT_REGEX = re.compile(r"\b[A-Z]{1,4}[0-9]{1,3}[A-Z]{1,3}[0-9]{4,8}\b")

###############################################################################
# SECTION NAMES
###############################################################################

FUNDING_SECTION_NAMES = {
    "funding",
    "financial support",
    "funding statement",
    "grant support",
    "support",
    "acknowledgements",
    "acknowledgments",
}

###############################################################################
# SOURCE WEIGHTS
###############################################################################

SOURCE_BASE_WEIGHTS = {
    "EuropePMC": 6,
    "PubMed": 5,
    "Crossref": 4,
    "OpenAlex": 3,
    "PMC_JATS": 2,
    "PMC_FullText": 1,
}

###############################################################################
# CONFIDENCE
###############################################################################


def confidence(source):

    if source in ("PubMed", "EuropePMC"):
        return "High"

    if source in ("Crossref", "OpenAlex", "PMC_JATS"):
        return "Medium"

    return "Low"


###############################################################################
# NORMALIZATION
###############################################################################


def normalize_funder(name):

    if not name:
        return None, None

    key = re.sub(r"[^a-z0-9 ]", "", str(name).lower()).strip()

    if key in FUNDER_NORMALIZATION:
        return FUNDER_NORMALIZATION[key]
    if "public health agency of canada" in key:
        return "Public Health Agency of Canada / CIHR / PCIRN", None
    if "canadian institutes of health research" in key:
        return "CIHR", None
    if "national science foundation" in key:
        return "NSF", None
    if "national natural science foundation of china" in key or "national nature science foundation of china" in key:
        return "National Natural Science Foundation of China", None
    if "university of michigan" in key:
        return "University of Michigan", None

    return name, None


###############################################################################
# IDENTIFIER NORMALIZATION
###############################################################################


def normalize_identifier(value):
    if value is None or pd.isna(value):
        return ""
    normalized = str(value).strip()
    if normalized.lower() in {"", "na", "nan", "none", "null"}:
        return ""
    return normalized


###############################################################################
# NIH PARSING
###############################################################################


def normalize_grant_id(grant_id):
    """Canonicalize common grant and award formatting variants."""
    if not grant_id:
        return None

    compact = re.sub(r"\s+", "", str(grant_id).upper()).strip()
    compact = re.sub(r"^(?:GRANT)?NO\.?:?", "", compact)
    match = re.fullmatch(
        rf"[0-9]?({NIH_ACTIVITY_CODES})([A-Z]{{2}})([0-9]{{5,6}})(?:-[0-9]{{2}}[A-Z0-9]*)?",
        compact,
    )
    if match:
        return "".join(match.groups())

    return compact or None


def is_plausible_grant_id(grant_id):
    normalized = normalize_grant_id(grant_id)
    if not normalized or len(normalized) > 48:
        return False
    if parse_nih_subagency(normalized):
        return True
    if UKRI_REGEX.fullmatch(normalized) or CONTRACT_GRANT_REGEX.fullmatch(normalized):
        return True
    return bool(re.search(r"\d", normalized) and re.search(r"[A-Z0-9]", normalized))


def parse_nih_subagency(grant_id):

    normalized = normalize_grant_id(grant_id)
    if not normalized:
        return None

    match = re.search(rf"(?:{NIH_ACTIVITY_CODES})([A-Z]{{2}})[0-9]{{5,6}}", normalized)
    return NIH_IC_MAP.get(match.group(1)) if match else None


###############################################################################
# GRANT EXTRACTION
###############################################################################


def split_structured_award_ids(grant_id, funder=None):
    normalized = normalize_grant_id(grant_id)
    funder_text = (funder or "").lower()
    if normalized and (
        "national natural science foundation" in funder_text
        or "national nature science foundation" in funder_text
    ):
        parts = re.findall(r"\d{8}", normalized)
        parts.extend(extract_grants(normalized))
        if len(parts) > 1:
            return list(dict.fromkeys(parts))
    return [normalized] if normalized and is_plausible_grant_id(normalized) else []


def extract_grants(text):

    if not text:
        return set()

    results = set()

    for regex in NIH_GRANT_REGEXES:
        results.update(normalize_grant_id(grant) for grant in regex.findall(text))

    results.update(UKRI_REGEX.findall(text))
    results.update(CONTRACT_GRANT_REGEX.findall(text.upper()))
    results.update(ALPHANUMERIC_GRANT_REGEX.findall(text.upper()))
    results.update(
        re.findall(
            r"\b(?:National Science Foundation|NSF)[^.;]*?grant\s+(\d{6,9})\b",
            text,
            flags=re.IGNORECASE,
        )
    )

    return {grant for grant in results if grant and is_plausible_grant_id(grant)}


def grant_evidence(grant_id, text, width=160):
    position = text.upper().find(str(grant_id).upper())
    if position < 0:
        return text[:500]
    start = max(0, position - width)
    end = min(len(text), position + len(str(grant_id)) + width)
    return text[start:end].strip()


NO_FUNDING_PATTERNS = (
    re.compile(r"\bdid not receive any specific grant\b", re.IGNORECASE),
    re.compile(r"\bno (?:external|specific) funding\b", re.IGNORECASE),
    re.compile(r"\bno funding was received\b", re.IGNORECASE),
    re.compile(r"\bnot receive any funding\b", re.IGNORECASE),
)

FUNDER_ALIASES = (
    ("national natural science foundation of china", "National Natural Science Foundation of China"),
    ("national nature science foundation of china", "National Natural Science Foundation of China"),
    ("national key r&d program of china", "National Key R&D Program of China"),
    ("national key research and development program of china", "National Key Research and Development Program of China"),
    ("strategic priority research program of the chinese academy of sciences", "Chinese Academy of Sciences Strategic Priority Research Program"),
    ("chinese academy of sciences", "Chinese Academy of Sciences"),
    ("science and technology innovation program of hunan province", "Hunan Province Science and Technology Innovation Program"),
    ("european research council", "European Research Council"),
    ("european science foundation", "European Science Foundation"),
    ("engineering and physical sciences research council", "EPSRC"),
    ("ibm research ai", "IBM Research AI"),
    ("national science foundation", "NSF"),
    ("knut and alice wallenberg foundation", "Knut and Alice Wallenberg Foundation"),
    ("max-planck-gesellschaft", "Max Planck Society"),
    ("max planck society", "Max Planck Society"),
    ("nomis foundation", "NOMIS Foundation"),
    ("wellcome trust", "Wellcome Trust"),
    ("novo nordisk foundation", "Novo Nordisk Foundation"),
    ("academy of finland", "Academy of Finland"),
    ("guangzhou medical university research enhancement project", "Guangzhou Medical University"),
    ("eu fp6", "European Union Sixth Framework Programme"),
)


def extract_prose_awards(text):
    normalized = re.sub(r"\s+", " ", text).strip()
    results = []
    for clause in re.split(r"[;.]", normalized):
        lowered = clause.lower()
        funder_starts = []
        for phrase, name in FUNDER_ALIASES:
            position = lowered.find(phrase)
            if position >= 0:
                funder_starts.append((position, name))
        funder_starts.sort()
        for index, (position, funder) in enumerate(funder_starts):
            end = funder_starts[index + 1][0] if index + 1 < len(funder_starts) else len(clause)
            segment = clause[position:end]
            grant_ids = set(extract_grants(segment))
            grant_ids.update(
                re.findall(r"\b[A-Z]{2,10}(?:[-/][A-Z0-9]+){2,}\b", segment.upper())
            )
            grant_ids.update(
                match.group(1).replace("NO.", "")
                for match in re.finditer(
                    r"(?:grant|project|award|contract)\s*(?:no\.?|number|#)?\s*:?\s*([A-Z]{0,8}\d[A-Z0-9/_-]{3,})",
                    segment,
                    flags=re.IGNORECASE,
                )
            )
            parenthesized = re.findall(
                r"\(([A-Z0-9/#,\s-]{5,80})\)", segment, flags=re.IGNORECASE
            )
            for value in parenthesized:
                grant_ids.update(
                    item.strip()
                    for item in re.split(r"[,/]|\band\b", value, flags=re.IGNORECASE)
                    if item.strip() and re.search(r"\d", item)
                )
            for grant_id in sorted(grant_ids):
                normalized_id = normalize_grant_id(grant_id)
                if normalized_id and is_plausible_grant_id(normalized_id):
                    results.append(
                        (funder, normalized_id, grant_evidence(normalized_id, segment))
                    )
    return results


###############################################################################
# API CLIENT
###############################################################################


class APIClient:
    def __init__(self, ncbi_api_key=None, cache_path=None, concurrency=5):
        self.sem = asyncio.Semaphore(concurrency)
        self.ncbi_api_key = ncbi_api_key
        self.cache_path = cache_path
        self.stats = Counter()
        self.ncbi_lock = asyncio.Lock()
        self.last_ncbi_request = 0.0
        self.cache = None

        if cache_path:
            cache_path.parent.mkdir(parents=True, exist_ok=True)
            self.cache = sqlite3.connect(cache_path)
            self.cache.execute(
                "CREATE TABLE IF NOT EXISTS responses "
                "(cache_key TEXT PRIMARY KEY, body TEXT NOT NULL, created_at REAL NOT NULL)"
            )
            self.cache.commit()

    def close(self):
        if self.cache:
            self.cache.close()

    def _cached_text(self, url):
        if not self.cache:
            return None
        cache_key = hashlib.sha256(url.encode()).hexdigest()
        row = self.cache.execute(
            "SELECT body FROM responses WHERE cache_key = ? AND created_at > ?",
            (cache_key, time.time() - CACHE_TTL_SECONDS),
        ).fetchone()
        if row:
            self.stats["cache_hits"] += 1
            return row[0]
        return None

    def _cache_text(self, url, body):
        if not self.cache:
            return
        cache_key = hashlib.sha256(url.encode()).hexdigest()
        self.cache.execute(
            "INSERT OR REPLACE INTO responses(cache_key, body, created_at) VALUES (?, ?, ?)",
            (cache_key, body, time.time()),
        )
        self.cache.commit()

    async def _throttle(self, url):
        if not urlsplit(url).netloc.endswith("ncbi.nlm.nih.gov"):
            return
        async with self.ncbi_lock:
            delay = NCBI_MIN_INTERVAL_SECONDS - (
                time.monotonic() - self.last_ncbi_request
            )
            if delay > 0:
                await asyncio.sleep(delay)
            self.last_ncbi_request = time.monotonic()

    async def _get_text(self, session, url):
        cached = self._cached_text(url)
        if cached is not None:
            return cached

        async with self.sem:
            for attempt in range(MAX_RETRIES + 1):
                await self._throttle(url)
                self.stats["requests"] += 1
                try:
                    async with session.get(
                        url,
                        headers={"User-Agent": USER_AGENT},
                        timeout=aiohttp.ClientTimeout(total=HTTP_TIMEOUT),
                    ) as response:
                        body = await response.text()
                        if response.status == 200:
                            self._cache_text(url, body)
                            return body
                        if response.status == 429:
                            self.stats["rate_limited"] += 1
                        elif response.status >= 500:
                            self.stats["server_errors"] += 1
                        else:
                            self.stats[f"http_{response.status}"] += 1
                            logger.warning(
                                "HTTP %s from %s",
                                response.status,
                                urlsplit(url).netloc,
                            )
                            return None
                        retry_after = response.headers.get("Retry-After")
                        try:
                            delay = float(retry_after) if retry_after else 2**attempt
                        except ValueError:
                            delay = 2**attempt
                except (aiohttp.ClientError, asyncio.TimeoutError) as exc:
                    self.stats["request_errors"] += 1
                    logger.debug(
                        "Request to %s failed with %s: %s",
                        urlsplit(url).netloc,
                        type(exc).__name__,
                        str(exc) or "no exception detail",
                    )
                    delay = 2**attempt

                if attempt < MAX_RETRIES:
                    await asyncio.sleep(delay)

        self.stats["failed_requests"] += 1
        return None

    async def get_json(self, session, url):
        text = await self._get_text(session, url)
        if not text:
            return None
        try:
            return json.loads(text)
        except json.JSONDecodeError:
            self.stats["invalid_json"] += 1
            logger.warning("Invalid JSON from %s", urlsplit(url).netloc)
            return None

    async def get_text(self, session, url):
        return await self._get_text(session, url)


###############################################################################
# ASSERTIONS
###############################################################################


def make_assertion(
    agency=None, subagency=None, grant_id=None, source=None, evidence=None
):

    return {
        "agency": agency,
        "subagency": subagency,
        "grant_id": grant_id,
        "source": source,
        "evidence": evidence,
    }


###############################################################################
# PUBMED
###############################################################################


async def query_pubmed(session, pmid, client):

    params = {
        "db": "pubmed",
        "id": pmid,
        "retmode": "xml",
        "tool": "FundingExtractor",
        "email": "allenbaron@som.umaryland.edu",
    }
    if client.ncbi_api_key:
        params["api_key"] = client.ncbi_api_key
    url = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"

    return await client.get_text(session, f"{url}?{urlencode(params)}")


def parse_pubmed_grants(xml):

    if not xml:
        return []

    try:
        root = ET.fromstring(xml)
    except ET.ParseError:
        return []

    out = []
    for grant in root.findall(".//Grant"):
        raw_grant_id = grant.findtext("GrantID")
        agency_name = (grant.findtext("Agency") or "").strip()
        acronym = (grant.findtext("Acronym") or "").strip().upper()
        norm_agency, norm_sub = normalize_funder(agency_name)
        grant_ids = split_structured_award_ids(raw_grant_id, agency_name) or [None]
        for grant_id in grant_ids:
            sub = parse_nih_subagency(grant_id) or NIH_IC_MAP.get(acronym) or norm_sub
            agency = "NIH" if sub else norm_agency
            out.append(
                make_assertion(
                    agency=agency,
                    subagency=sub,
                    grant_id=grant_id,
                    source="PubMed",
                    evidence=f"{agency_name} {grant_id or raw_grant_id or ''}".strip(),
                )
            )

    return out


###############################################################################
# EUROPE PMC
###############################################################################


async def query_europepmc(session, query, client):

    params = {"query": query, "format": "json", "resultType": "core"}
    url = "https://www.ebi.ac.uk/europepmc/webservices/rest/search"

    return await client.get_json(session, f"{url}?{urlencode(params)}")


def parse_europepmc(data):

    if not data:
        return []

    results = []
    records = data.get("resultList", {}).get("result", [])

    for record in records:
        grants = record.get("grantsList", {}).get("grant", [])
        for grant in grants:
            raw_grant_id = grant.get("grantId")
            agency_name = (grant.get("agency") or "").strip()
            acronym = (grant.get("acronym") or "").strip().upper()
            norm_agency, norm_subagency = normalize_funder(agency_name)
            grant_ids = split_structured_award_ids(raw_grant_id, agency_name) or [None]
            for grant_id in grant_ids:
                subagency = (
                    parse_nih_subagency(grant_id)
                    or NIH_IC_MAP.get(acronym)
                    or norm_subagency
                )
                agency = "NIH" if subagency else norm_agency
                results.append(
                    make_assertion(
                        agency=agency,
                        subagency=subagency,
                        grant_id=grant_id,
                        source="EuropePMC",
                        evidence=f"{agency_name} {grant_id or raw_grant_id or ''}".strip(),
                    )
                )

    return results


###############################################################################
# CROSSREF
###############################################################################


async def query_crossref(session, doi, client):

    return await client.get_json(
        session, f"https://api.crossref.org/works/{quote(doi, safe='')}"
    )


def parse_crossref(data):

    out = []

    if not data:
        return out

    funders = data.get("message", {}).get("funder", [])

    for f in funders:
        agency, subagency = normalize_funder(f.get("name"))

        awards = f.get("award") or []

        if not awards:
            out.append(
                make_assertion(
                    agency=agency,
                    subagency=subagency,
                    grant_id=None,
                    source="Crossref",
                    evidence=f.get("name"),
                )
            )

        else:
            for award in awards:
                grant_ids = split_structured_award_ids(award, f.get("name")) or [None]
                for grant_id in grant_ids:
                    sub = parse_nih_subagency(grant_id) or subagency
                    normalized_agency = "NIH" if sub else agency
                    out.append(
                        make_assertion(
                            agency=normalized_agency,
                            subagency=sub,
                            grant_id=grant_id,
                            source="Crossref",
                            evidence=f"{f.get('name')} {grant_id or award}".strip(),
                        )
                    )

    return out


###############################################################################
# OPENALEX
###############################################################################


async def query_openalex(session, doi, client):

    doi = normalize_identifier(doi).lower().removeprefix("https://doi.org/")
    work_id = quote(f"https://doi.org/{doi}", safe="")

    return await client.get_json(session, f"https://api.openalex.org/works/{work_id}")


def parse_openalex(data):

    results = []

    if not data:
        return results

    for g in data.get("grants", []):
        funder = g.get("funder_display_name")

        agency, subagency = normalize_funder(funder)

        raw_grant_id = g.get("award_id")
        grant_ids = split_structured_award_ids(raw_grant_id, funder) or [None]
        for gid in grant_ids:
            parsed_subagency = parse_nih_subagency(gid) or subagency
            normalized_agency = "NIH" if parsed_subagency else agency
            results.append(
                make_assertion(
                    agency=normalized_agency,
                    subagency=parsed_subagency,
                    grant_id=gid,
                    source="OpenAlex",
                    evidence=funder,
                )
            )

    return results


###############################################################################
# PMC FULLTEXT
###############################################################################


async def query_pmc_fulltext(session, pmcid, client):

    url = (
        "https://www.ebi.ac.uk/europepmc/webservices/rest/"
        f"{quote(pmcid, safe='')}/fullTextXML"
    )

    return await client.get_text(session, url)


def parse_pmc_sections(xml):

    if not xml:
        return []

    try:
        root = ET.fromstring(xml)
    except ET.ParseError as exc:
        logger.warning("Could not parse PMC full-text XML: %s", exc)
        return []

    passages = []
    for element in root.findall(".//ack"):
        paragraphs = [" ".join("".join(p.itertext()).split()) for p in element.findall(".//p")]
        text = " ".join(paragraph for paragraph in paragraphs if paragraph)
        if not text:
            text = " ".join("".join(element.itertext()).split())
        if text:
            passages.append(text)
    for element in root.findall(".//sec"):
        title_element = element.find("title")
        title = (
            " ".join("".join(title_element.itertext()).split())
            if title_element is not None
            else ""
        )
        if title.lower().strip() in FUNDING_SECTION_NAMES:
            paragraphs = [" ".join("".join(p.itertext()).split()) for p in element.findall(".//p")]
            text = " ".join(paragraph for paragraph in paragraphs if paragraph)
            if not text:
                text = " ".join("".join(element.itertext()).split())
            if text:
                passages.append(text)

    return list(dict.fromkeys(passages))


def infer_fulltext_agency(grant_id, evidence):
    subagency = parse_nih_subagency(grant_id)
    if subagency:
        return "NIH", subagency
    if re.fullmatch(r"\d{6,9}", grant_id or "") and (
        "NSF" in evidence.upper()
        or "NATIONAL SCIENCE FOUNDATION" in evidence.upper()
    ):
        return "NSF", None
    if re.match(r"^N0[0-9][A-Z]{2}", grant_id or "") or "NIH" in evidence.upper():
        match = re.match(r"^N0[0-9]([A-Z]{2})", grant_id or "")
        return "NIH", NIH_IC_MAP.get(match.group(1)) if match else None
    if "NSF" in evidence.upper() or "NATIONAL SCIENCE FOUNDATION" in evidence.upper():
        return "NSF", None
    return normalize_funder(evidence)[0], None


def parse_pmc_funding(xml):

    if not xml:
        return []

    try:
        root = ET.fromstring(xml)
    except ET.ParseError:
        return []

    assertions = []
    funding_groups = root.findall(".//funding-group")
    for group in funding_groups:
        awards = group.findall(".//award-group") or [group]
        for award in awards:
            funders = [
                " ".join("".join(item.itertext()).split())
                for item in award.findall(".//institution")
            ]
            if not funders:
                funders = [
                    " ".join("".join(item.itertext()).split())
                    for item in award.findall(".//funding-source")
                ]
            explicit_awards = [
                " ".join("".join(item.itertext()).split())
                for item in award.findall(".//award-id")
            ]
            evidence = " ".join("".join(award.itertext()).split())
            grants = set(extract_grants(evidence))
            grants.update(
                normalize_grant_id(value)
                for value in explicit_awards
                if value and re.search(r"\d", value)
            )

            if grants:
                for grant_id in sorted(grants):
                    agency, subagency = infer_fulltext_agency(grant_id, evidence)
                    assertions.append(
                        make_assertion(
                            agency=agency,
                            subagency=subagency,
                            grant_id=grant_id,
                            source="PMC_JATS",
                            evidence=grant_evidence(grant_id, evidence),
                        )
                    )
            elif funders:
                for funder in funders:
                    agency, subagency = normalize_funder(funder)
                    assertions.append(
                        make_assertion(
                            agency=agency,
                            subagency=subagency,
                            grant_id=None,
                            source="PMC_JATS",
                            evidence=funder[:1000],
                        )
                    )

    for text in parse_pmc_sections(xml):
        for grant_id in sorted(extract_grants(text)):
            agency, subagency = infer_fulltext_agency(grant_id, text)
            assertions.append(
                make_assertion(
                    agency=agency,
                    subagency=subagency,
                    grant_id=grant_id,
                    source="PMC_FullText",
                    evidence=grant_evidence(grant_id, text),
                )
            )

        for funder, grant_id, evidence in extract_prose_awards(text):
            agency, subagency = normalize_funder(funder)
            assertions.append(
                make_assertion(
                    agency=agency,
                    subagency=subagency,
                    grant_id=grant_id,
                    source="PMC_FullText",
                    evidence=evidence,
                )
            )

        lowered = text.lower()
        no_funding = next((p.search(text) for p in NO_FUNDING_PATTERNS if p.search(text)), None)
        if no_funding:
            assertions.append(
                make_assertion(
                    source="PMC_FullText",
                    evidence=grant_evidence(no_funding.group(0), text),
                )
            )
        named_funders = []
        if "public health agency of canada" in lowered:
            named_funders.append("Public Health Agency of Canada / CIHR / PCIRN")
        if any(
            phrase in lowered
            for phrase in (
                "rackham pilot research",
                "undergraduate research opportunity program",
            )
        ):
            named_funders.append("University of Michigan")
        for agency in named_funders:
            assertions.append(
                make_assertion(
                    agency=agency,
                    source="PMC_FullText",
                    evidence=text[:1000],
                )
            )

    return assertions


###############################################################################
# CONSOLIDATION
###############################################################################


def information_score(record):

    score = SOURCE_BASE_WEIGHTS.get(record["source"], 0)

    if record["grant_id"]:
        score += 4

    if record["subagency"]:
        score += 2

    if record["agency"]:
        score += 1

    return score


def consolidate(assertions):

    groups = {}

    for assertion in assertions:
        grant_id = normalize_grant_id(assertion["grant_id"])
        key = (
            ("grant", grant_id)
            if grant_id
            else (
                "funder",
                assertion["agency"],
                assertion["subagency"],
            )
        )

        if key not in groups:
            groups[key] = {
                "grant_id": grant_id,
                "agency": assertion["agency"],
                "subagency": assertion["subagency"],
                "evidence": assertion["evidence"],
                "sources": [assertion["source"]],
                "best_score": information_score(assertion),
            }
        else:
            groups[key]["sources"].append(assertion["source"])

            score = information_score(assertion)
            if score > groups[key]["best_score"]:
                groups[key]["best_score"] = score
                groups[key]["agency"] = assertion["agency"]
                groups[key]["subagency"] = assertion["subagency"]
                if assertion["evidence"]:
                    groups[key]["evidence"] = assertion["evidence"]

    results = []

    for group in groups.values():
        v = group
        unique_sources = sorted(
            set(v["sources"]), key=lambda s: SOURCE_BASE_WEIGHTS.get(s, 0), reverse=True
        )

        results.append(
            {
                "grant_id": v["grant_id"],
                "agency": v["agency"],
                "subagency": v["subagency"],
                "sources": "|".join(unique_sources),
                "evidence": v["evidence"],
                "confidence": confidence(unique_sources[0]),
            }
        )

    return results


###############################################################################
# RECORD PROCESSING
###############################################################################


async def process_record(row, session, client, on_lookup=None):

    def lookup_complete():
        if on_lookup:
            on_lookup()

    pmid = normalize_identifier(row.get("pmid"))

    pmcid = normalize_identifier(row.get("pmcid")).upper()
    if pmcid and not pmcid.startswith("PMC"):
        pmcid = f"PMC{pmcid}"

    doi = normalize_identifier(row.get("doi"))
    doi = re.sub(r"^https?://(?:dx\.)?doi\.org/", "", doi, flags=re.IGNORECASE)

    assertions = []
    matched_sources = []

    ###########################################################################
    # PUBMED
    ###########################################################################

    if pmid:
        xml = await query_pubmed(session, pmid, client)
        if xml:
            try:
                pubmed_root = ET.fromstring(xml)
                if pubmed_root.findall(".//PubmedArticle"):
                    matched_sources.append("PubMed")
            except ET.ParseError:
                logger.warning("Could not parse PubMed XML for PMID %s", pmid)

        assertions.extend(parse_pubmed_grants(xml))
        lookup_complete()

    ###########################################################################
    # EUROPE PMC
    ###########################################################################

    query = None

    if pmid:
        query = f"EXT_ID:{pmid}"

    elif doi:
        query = f'DOI:"{doi}"'

    if query:
        data = await query_europepmc(session, query, client)
        if data and data.get("resultList", {}).get("result"):
            matched_sources.append("EuropePMC")

        assertions.extend(parse_europepmc(data))
        lookup_complete()

    ###########################################################################
    # CROSSREF
    ###########################################################################

    if doi:
        data = await query_crossref(session, doi, client)
        if data and data.get("message", {}).get("DOI"):
            matched_sources.append("Crossref")

        assertions.extend(parse_crossref(data))
        lookup_complete()

    ###########################################################################
    # OPENALEX
    ###########################################################################

    if doi:
        data = await query_openalex(session, doi, client)
        if data and data.get("id"):
            matched_sources.append("OpenAlex")

        assertions.extend(parse_openalex(data))
        lookup_complete()

    ###########################################################################
    # FULLTEXT SUPPLEMENT
    ###########################################################################

    if pmcid:
        doc = await query_pmc_fulltext(session, pmcid, client)
        if doc:
            try:
                fulltext_root = ET.fromstring(doc)
                if fulltext_root.tag.endswith("article") or fulltext_root.findall(
                    ".//article"
                ):
                    matched_sources.append("PMC_JATS")
            except ET.ParseError:
                logger.warning("Could not parse PMC full text for %s", pmcid)

        assertions.extend(parse_pmc_funding(doc))
        lookup_complete()

    ###########################################################################

    final = consolidate(assertions)
    if not final:
        ordered_matches = sorted(
            set(matched_sources),
            key=lambda source: SOURCE_BASE_WEIGHTS.get(source, 0),
            reverse=True,
        )
        final = [
            {
                "agency": None,
                "subagency": None,
                "grant_id": None,
                "confidence": None,
                "sources": "|".join(ordered_matches),
                "evidence": None,
            }
        ]

    publication = {
        "pmid": pmid,
        "pmcid": pmcid,
        "doi": doi,
        "scopus_eid": normalize_identifier(row.get("scopus_eid")),
        "title": normalize_identifier(row.get("title")),
    }
    rows = []

    for r in final:
        rows.append(
            {
                **publication,
                "agency": normalize_identifier(r["agency"]),
                "subagency": normalize_identifier(r["subagency"]),
                "grant_id": normalize_identifier(r["grant_id"]),
                "confidence": normalize_identifier(r["confidence"]),
                "sources": normalize_identifier(r["sources"]),
                "evidence_text": normalize_identifier(r["evidence"]),
            }
        )

    return rows


###############################################################################
# MAIN
###############################################################################


def expected_lookup_count(row):
    pmid = normalize_identifier(row.get("pmid"))
    pmcid = normalize_identifier(row.get("pmcid"))
    doi = normalize_identifier(row.get("doi"))
    return int(bool(pmid)) + int(bool(pmid or doi)) + 2 * int(bool(doi)) + int(bool(pmcid))


async def run(
    df, ncbi_api_key=None, cache_path=None, concurrency=5, show_progress=True
):

    client = APIClient(
        ncbi_api_key=ncbi_api_key,
        cache_path=cache_path,
        concurrency=concurrency,
    )
    total_publications = len(df)
    completed_publications = 0
    progress = None
    request_status = None
    if show_progress and tqdm_asyncio is not None:
        progress = tqdm_asyncio(
            total=total_publications,
            desc=f"Publications = 0/{total_publications}",
            bar_format="Progress: |{bar:40}| {percentage:3.0f}%   {desc}",
            mininterval=0.1,
            unit="publication",
            position=0,
        )
        request_status = tqdm_asyncio(
            total=1,
            desc="  Requests = 0, Cache = 0",
            bar_format="{desc}",
            position=1,
        )

    def update_progress_display():
        if progress is not None:
            progress.set_description_str(
                f"Publications = {completed_publications}/{total_publications}",
                refresh=False,
            )
            request_status.set_description_str(
                f"  Requests = {client.stats['requests']}, "
                f"Cache = {client.stats['cache_hits']}",
                refresh=False,
            )
            progress.refresh()
            request_status.refresh()

    async def process_with_progress(row):
        nonlocal completed_publications
        lookup_count = expected_lookup_count(row)
        completed_lookups = 0

        def lookup_complete():
            nonlocal completed_lookups
            completed_lookups += 1
            if progress is not None and lookup_count:
                progress.update(1 / lookup_count)
                update_progress_display()

        try:
            return await process_record(row, session, client, lookup_complete)
        finally:
            if progress is not None:
                remaining = max(0.0, 1 - completed_lookups / max(1, lookup_count))
                progress.update(remaining)
                completed_publications += 1
                update_progress_display()

    try:
        async with aiohttp.ClientSession() as session:
            update_progress_display()
            tasks = [process_with_progress(row) for _, row in df.iterrows()]
            completed = []
            for task in asyncio.as_completed(tasks):
                try:
                    completed.append(await task)
                except Exception as exc:
                    completed.append(exc)

        if progress is not None:
            progress.n = total_publications
            progress.refresh()

        results = []
        for item in completed:
            if isinstance(item, Exception):
                client.stats["record_errors"] += 1
                logger.error(
                    "Record processing failed: %s",
                    item,
                    exc_info=(type(item), item, item.__traceback__),
                )
            else:
                results.extend(item)

        return pd.DataFrame(results), client.stats
    finally:
        if progress is not None:
            progress.close()
        if request_status is not None:
            request_status.close()
        client.close()


def main():

    parser = argparse.ArgumentParser(
        description=(
            "Extract publication funding metadata from structured APIs and "
            "available full text."
        )
    )
    parser.add_argument(
        "-i", "--input",
        required=True,
        type=Path,
        metavar="TSV_PATH",
        help="Input publication TSV containing identifier and title columns.",
    )
    parser.add_argument(
        "-o", "--output",
        type=Path,
        metavar="TSV_PATH",
        help="Output funding TSV; required unless --check is used.",
    )
    parser.add_argument(
        "-l", "--limit", type=int, metavar="N", help="Process only the first N input rows."
    )
    parser.add_argument(
        "--concurrency",
        type=int,
        default=MAX_CONCURRENT_REQUESTS,
        metavar="N",
        help=f"Maximum simultaneous requests (default: {MAX_CONCURRENT_REQUESTS}).",
    )
    parser.add_argument(
        "--cache",
        type=Path,
        metavar="SQLITE_PATH",
        help=(
            "Path to the SQLite response cache. Successful API responses are reused "
            f"for 30 days (default: {DEFAULT_CACHE_NAME} next to --input)."
        ),
    )
    parser.add_argument(
        "--no-cache",
        action="store_true",
        help="Bypass the response cache and make fresh API requests.",
    )
    parser.add_argument(
        "--no-progress",
        action="store_true",
        help="Disable the progress bar for non-interactive runs.",
    )
    parser.add_argument(
        "--log-level",
        choices=("DEBUG", "INFO", "WARNING", "ERROR"),
        default="INFO",
        help="Diagnostic logging level (default: INFO).",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Validate dependencies, input, and credential availability without network access.",
    )
    args = parser.parse_args()
    logging.basicConfig(
        level=getattr(logging, args.log_level), format="%(levelname)s: %(message)s"
    )

    if not args.check and not args.output:
        parser.error("--output is required unless --check is used")

    if aiohttp is None:
        parser.error(
            "aiohttp is required; install it with 'python -m pip install aiohttp'"
        )
    if args.check and tqdm_asyncio is None:
        logger.warning("tqdm is unavailable; progress bars will be disabled")
    elif tqdm_asyncio is None and not args.no_progress:
        parser.error(
            "tqdm is required for the progress bar; install it with "
            "'python -m pip install tqdm' or use --no-progress"
        )

    if not args.input.is_file():
        parser.error(f"input file does not exist: {args.input}")

    if args.limit is not None and args.limit < 1:
        parser.error("--limit must be at least 1")
    if args.concurrency < 1:
        parser.error("--concurrency must be at least 1")

    try:
        df = read_input_tsv(args.input)
    except (OSError, UnicodeError, ValueError) as exc:
        parser.error(f"could not read input TSV: {exc}")
    missing = sorted({"pmid", "pmcid", "doi"} - set(df.columns))
    if missing:
        parser.error(f"input is missing identifier columns: {', '.join(missing)}")
    if args.limit:
        df = df.head(args.limit)

    ncbi_api_key = get_ncbi_api_key()
    if args.check:
        credential_status = "available" if ncbi_api_key else "not found"
        print(
            f"Ready: {len(df):,} input rows; aiohttp installed; "
            f"{CREDENTIAL_NAME} {credential_status}"
        )
        return

    if not ncbi_api_key:
        parser.error(
            f"NCBI key {CREDENTIAL_NAME} was not found in Mac Keychain. "
            f"Store it with: security add-generic-password -s {CREDENTIAL_NAME} "
            f"-a $USER -w"
        )

    cache_path = None if args.no_cache else args.cache
    if cache_path is None and not args.no_cache:
        cache_path = args.input.parent / DEFAULT_CACHE_NAME
    results, stats = asyncio.run(
        run(
            df,
            ncbi_api_key=ncbi_api_key,
            cache_path=cache_path,
            concurrency=args.concurrency,
            show_progress=not args.no_progress,
        )
    )
    results = results.reindex(columns=EXPECTED_OUTPUT_COLUMNS)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    temporary_output = args.output.with_suffix(f"{args.output.suffix}.tmp")
    results.to_csv(temporary_output, sep="\t", index=False)
    temporary_output.replace(args.output)

    funding_rows = results["agency"].astype(str).str.len().gt(0) | results[
        "grant_id"
    ].astype(str).str.len().gt(0)
    funded_publications = results.loc[
        funding_rows, ["pmid", "pmcid", "doi", "scopus_eid", "title"]
    ].drop_duplicates()
    summary = ", ".join(f"{key}={value}" for key, value in sorted(stats.items()))
    print(
        f"Saved {len(results):,} output rows for {len(df):,} publications; "
        f"funding identified for {len(funded_publications):,}"
    )


if __name__ == "__main__":
    main()
