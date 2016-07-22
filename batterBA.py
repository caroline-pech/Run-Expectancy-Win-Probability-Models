from bs4 import BeautifulSoup
import urllib2
import re
import csv

# scrape player's batting average from espn 
# input is player's name and espn team abbreviation -- see teams.csv table 
def get_ba(player, team):
	url = 'http://espn.go.com/mlb/team/stats/batting/_/name/%s' % (team) 
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	table = soup.find(text = re.compile("NAME")).find_parent("table")
	rows = table.findAll('tr')
	player = table.find(text = re.compile(player)).find_parent("tr")
	cells = player.findAll('td')
	# stat is stat line of all players stats
	stat = [cell.text.strip('\n') for cell in cells]
	# stat[13] is player's batting average
	BA = float(stat[13])
	return(BA)