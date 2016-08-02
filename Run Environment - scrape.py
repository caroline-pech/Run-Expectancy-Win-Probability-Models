import csv
from bs4 import BeautifulSoup
import urllib2
import re


# script to scrape current run environment from current mlb year 
def get_run_environment():
	url = 'http://espn.go.com/mlb/stats/team/_/stat/batting'
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	# after parsing through the mlb team stats page from the current year, retrieve the row with averages between NL and AL 
	# and return runs per game as the run environment 
	table = soup.find(text = re.compile("Sortable Batting")).find_parent("table")
	rows = table.findAll('tr')
	x = table.find(text = re.compile('Major League Baseball')).find_parent("tr")
	stat = [i.text.strip('\n') for i in x]
	return(float(stat[3])/float(stat[1]))