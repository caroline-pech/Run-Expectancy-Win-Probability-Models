from bs4 import BeautifulSoup
import urllib2
import re
import csv

# script to get the percentage of singles, doubles, triples, and homeruns from a specific player's hits
# input is player's name and espn team abbreviation -- see teams.csv table 
def get_percentages(player, team):
	url = 'http://espn.go.com/mlb/team/stats/batting/_/name/%s' % (team) 
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	table = soup.find(text = re.compile("NAME")).find_parent("table")
	rows = table.findAll('tr')
	try:
		player = table.find(text = re.compile(player)).find_parent("tr")
		cells = player.findAll('td')
		stat = [cell.text.strip('\n') for cell in cells]
		# stat is line of player's stats
		# calculations below find percents of singles/hits, doubles/hits, etc
		single = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
		psingle = round(single/float(stat[2]), 4)
		pdouble = round(float(stat[5])/float(stat[2]), 4)
		ptriple = round(float(stat[6])/float(stat[2]), 4)
		phr = round(float(stat[7])/float(stat[2]), 4)
		pOUT = round(1 - (psingle + pdouble + ptriple + phr), 4)
		return(psingle,pdouble,ptriple,phr,pOUT)
	except:
		player = table.find(text = re.compile('Totals')).find_parent("tr")
		cells = player.findAll('td')
		stat = [cell.text.strip('\n') for cell in cells]
		# stat is line of player's stats
		# calculations below find percents of singles/hits, doubles/hits, etc
		single = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
		psingle = round(single/float(stat[2]), 4)
		pdouble = round(float(stat[5])/float(stat[2]), 4)
		ptriple = round(float(stat[6])/float(stat[2]), 4)
		phr = round(float(stat[7])/float(stat[2]), 4)
		pOUT = round(1 - (psingle + pdouble + ptriple + phr), 4)
		return(psingle,pdouble,ptriple,phr,pOUT)