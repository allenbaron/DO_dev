# modified from https://github.com/zenodo/zenodo/issues/1463

import requests

# Fill these in...
repo = "DiseaseOntology/PathogenTransmissionOntology"
token = "...webhook_token_for_TRANS_zenodo..."

# repo = "DiseaseOntology/SymptomOntology"
#token = "...webhook_token_for_TRANS_zenodo..."


headers = {"Accept": "application/vnd.github.v3+json"}

repo_response = requests.get(f"https://api.github.com/repos/{repo}", headers=headers)
release_response = requests.get(f"https://api.github.com/repos/{repo}/releases", headers=headers)

releases = release_response.json()
releases.reverse()  # Puts them in order from oldest to newest

success = []

for release in releases:  # Publish all old releases
    tn = release['tag_name']
    if tn in success:
        print(f'Skipping {tn}...', end=" ", flush=True)
        continue
    user = input(f'Next release is {tn}, continue? y/n ')
    if user != 'y':
        print('breaking...')
        break

    payload = {"action": "published", "release": release, "repository": repo_response.json()}
    submit_response = requests.post(
        f"https://zenodo.org/api/hooks/receivers/github/events/?access_token={token}",
        json=payload
    )
    print(submit_response, end=" ", flush=True)
    user = input("okay to continue? y/n ")
    if user == "y":
        success.append(tn)
    else:
        print('breaking...')
        break

print(success)
