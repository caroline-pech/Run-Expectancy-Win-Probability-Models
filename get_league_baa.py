from bs4 import BeautifulSoup
import urllib2
import re
import csv

# script to scrape current league batting average against
# input 'AL' for American League batting avg or 'NL' for National League batting avg against
def get_league_baa(league):
	url = 'http://espn.go.com/mlb/stats/team/_/stat/pitching/year/'
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	table = soup.find(text = re.compile("Sortable Pitching")).find_parent("table")
	rows = table.findAll('tr')
	if league == 'AL': 
		a = 'American League'
	elif league == 'NL':
		a = 'National League'
	x = table.find(text = re.compile(a)).find_parent("tr")
	stat = [i.text.strip('\n') for i in x]
	# stat is stat line for league averages. stat[14] is league batting avg against
	league = stat[14]
	return(league)