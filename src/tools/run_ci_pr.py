from github import Github
import requests
import json
import click
import sys
import os

# using an access token
@click.command()
@click.option('--commit_short', default="", required=True, help='Short Sha of commit.')
@click.option('--repo', default="gladiaio/gladia", show_default=True, required=True, help='Repo to scan')
@click.option('--gh_token', default="", required=True, help='Github Token')
def commit_should_run(commit_short="", repo="", gh_token=""):
  
  g = Github(gh_token)
  repo = g.get_repo(repo)
  pulls = repo.get_pulls(state='open', sort='created', base='main')
  
  #0 is ok
  #1 is nok
  skip_build = True
  for pr in pulls:
    if "WIP" not in pr.title:    
      pr_number = pr.number
      print(f"looking into {pr.title}")
      req = requests.get(f'https://api.github.com/repos/gladiaio/gladia/pulls/{pr_number}/commits')
      commits = json.loads(req.text)
      for commit in commits:
        if commit["sha"][0:6] == commit_short:
          skip_build = False

  if skip_build:
    print("Shouldn't run")
    sys.exit(1)
  else:
    print("should run")
    sys.exit(0)

  return not skip_build

if __name__ == '__main__':
    commit_should_run()