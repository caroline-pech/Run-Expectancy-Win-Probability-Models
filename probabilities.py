import csv
from bs4 import BeautifulSoup
import urllib2
import re
# open the csv with run expectancies for a given state and count and append the states to one list and the corresponding run expectancies to a seperate list
states = []
runsexp = []
with open('1990.2015.RE.States.Count.csv', 'rb') as file:
	counts = csv.reader(file, delimiter = ',')
	for row in counts:
		states.append(str(row[0]))
		runsexp.append(row[1:])
# zip the two lists into a dictionary
count_state = dict(zip(states, runsexp))
# dictionary with yahoo abbreviations for all MLB teams and espn abbreviations
yahoo_abv = {'Angels':'laa', 'Orioles':'bal', 'Red Sox':'bos', 'White Sox':'chw', 'Indians':'cle', 'Tigers':'det', 'Astros':'hou', 'Royals':'kan', 'Twins':'min', 'Yankees':'nyy', 'Athletics':'oak', 'Mariners':'sea', 'Rays':'tam', 'Rangers':'tex', 'Blue Jays':'tor', 'Diamondbacks':'ari', 'Braves':'atl', 'Cubs':'chc', 'Reds':'cin', 'Rockies':'col', 'Dodgers':'lad', 'Marlins':'mia', 'Brewers':'mil', 'Mets':'nym', 'Phillies':'phi', 'Pirates':'pit', 'Padres':'sdg', 'Giants':'sfo', 'Cardinals':'stl', 'Nationals':'was'}
batter_abv ={'Angels':'laa', 'Orioles':'bal', 'Red Sox':'bos', 'White Sox':'chw', 'Indians':'cle', 'Tigers':'det', 'Astros':'hou', 'Royals':'kc', 'Twins':'min', 'Yankees':'nyy', 'Athletics':'oak', 'Mariners':'sea', 'Rays':'tb', 'Rangers':'tex', 'Blue Jays':'tor', 'Diamondbacks':'ari', 'Braves':'atl', 'Cubs':'chc', 'Reds':'cin', 'Rockies':'col', 'Dodgers':'la', 'Marlins':'mia', 'Brewers':'mil', 'Mets':'nym', 'Phillies':'phi', 'Pirates':'pit', 'Padres':'sd', 'Giants':'sf', 'Cardinals':'stl', 'Nationals':'was'}
# AL teams in AL list and NL teams in NL list
AL = ['Angels','Orioles','Red Sox','White Sox','Indians','Tigers','Astros','Royals','Twins','Yankees','Athletics','Mariners','Rays','Rangers','Blue Jays']
NL = ['Diamondbacks','Braves','Cubs','Reds','Rockies','Dodgers','Marlins','Brewers','Mets','Phillies','Pirates','Padres','Giants','Cardinals','Nationals']
# find the difference in the run expectancy with the given count versus the base run expectancy without count analysis and scale that to the probability for the event
def get_count_probability(state, count, probability):
	# index the count and pull the run expectancy using that index
	index = count_state[''].index(count)
	runs = count_state[state][index]
	# get the base run expectancy without count analysis for comparison
	base = count_state[state][0]
	# calculate the difference between the base run expectancy and the run expectancy with the count
	delta = float(runs) - float(base)
	# if the difference is positive (with count has higher run expectancy), return scaled value of difference * probability of the event occuring
	if delta > 0:
		return((1+(delta/10)) * probability)
	# if the difference is negative (with count has lower run expectancy), return lower percentage of the probability of the event scaled to the difference
	elif delta < 0:
		x = (1+abs(delta/10)) * probability
		dif = x - probability
		return(probability - dif)
# get batting average depending on the circumstance
def get_ba_by_state(state, batter, batter_team):
	# depending on the state, pull stats from espn page dedicated to that type of event
	if state in ['000 0','000 1','000 2']:
		# none on
		url = 'http://espn.go.com/mlb/team/stats/batting/_/name/%s/split/37' % (batter_team)
	elif state in ['100 0', '100 1', '100 2']:
		# runners on		
		url = 'http://espn.go.com/mlb/team/stats/batting/_/name/%s/split/38' % (batter_team) 
	elif state in ['111 0', '111 1', '111 2']:
		# bases loaded
		url = 'http://espn.go.com/mlb/team/stats/batting/_/name/%s/split/94' % (batter_team) 
	elif state in ['110 0', '110 1', '101 0', '101 1', '011 0', '011 1','010 0','010 1','001 0','001 1']:
		# runners in scoring position
		url = 'http://espn.go.com/mlb/team/stats/batting/_/name/%s/split/39' % (batter_team) 
	elif state in ['110 2', '101 2', '011 2', '010 2', '001 2']:
		# runners in scoring position with 2 outs
		url = 'http://espn.go.com/mlb/team/stats/batting/_/name/%s/split/185' % (batter_team) 
	# parse through the page and get the stats table
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	table = soup.find(text = re.compile("NAME")).find_parent("table")
	rows = table.findAll('tr')
	# try to pull the players statline (if player is not in the table, go to except block)
	try:
		player = table.find(text = re.compile(batter)).find_parent("tr")
		cells = player.findAll('td')
		stat = [cell.text.strip('\n') for cell in cells]
		# calculations below find percents of singles/PA, doubles/PA, etc
		single = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
		psingle = single/(float(stat[2])+float(stat[10]))
		pdouble = float(stat[5])/(float(stat[2])+float(stat[10]))
		ptriple = float(stat[6])/(float(stat[2])+float(stat[10]))
		phr = float(stat[7])/(float(stat[2])+float(stat[10]))
		pWALK = float(stat[10])/(float(stat[2])+float(stat[10]))
		# if less than 50 plate appearances, use team averages OR
		# if player has not hit a single, double, triple, hr, or been walked, use team averages
		if ((float(stat[2]) + float(stat[10])) < 50) or (psingle == 0 and pdouble == 0 and ptriple == 0 and phr == 0 and pWALK == 0):
			player = table.find(text = re.compile('Totals')).find_parent("tr")
			cells = player.findAll('td')
			stat = [cell.text.strip('\n') for cell in cells]
			# calculations below find percents of singles/PA, doubles/PA, etc
			single2 = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
			psingle2 = single2/(float(stat[2])+float(stat[10]))
			pdouble2 = float(stat[5])/(float(stat[2])+float(stat[10]))
			ptriple2 = float(stat[6])/(float(stat[2])+float(stat[10]))
			phr2 = float(stat[7])/(float(stat[2])+float(stat[10]))
			pWALK2 = float(stat[10])/(float(stat[2])+float(stat[10]))
			return((psingle + psingle2)/2, (pdouble + pdouble2)/2, (ptriple + ptriple2)/2, (phr + phr2)/2, (pWALK + pWALK2/2))
		# otherwise return original calculations
		else:
			return(psingle,pdouble,ptriple,phr,pWALK)
	# if player not in ESPN table, use team averages 
	except:
		player = table.find(text = re.compile('Totals')).find_parent("tr")
		cells = player.findAll('td')
		stat = [cell.text.strip('\n') for cell in cells]
		# calculations below find percents of singles/PA, doubles/PA, etc
		single = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
		psingle = single/(float(stat[2])+float(stat[10]))
		pdouble = float(stat[5])/(float(stat[2])+float(stat[10]))
		ptriple = float(stat[6])/(float(stat[2])+float(stat[10]))
		phr = float(stat[7])/(float(stat[2])+float(stat[10]))
		pWALK = float(stat[10])/(float(stat[2])+float(stat[10]))
		return(psingle,pdouble,ptriple,phr,pWALK)
# get percent of doubles and triples hit in a given year for function get_pitcher_percentages below
def get_pitcher_doubles_triples():
	url = 'http://www.espn.com/mlb/stats/team/_/stat/batting/year/'
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	# get ESPN table of batting stats
	table = soup.find(text = re.compile("Sortable Batting")).find_parent("table")
	rows = table.findAll('tr')
	player = table.find(text = re.compile('Major League Baseball')).find_parent("tr")
	cells = player.findAll('td')
	# pull stat line for all of MLB and calculate doubles/hits and triples/hits
	stat = [cell.text.strip('\n') for cell in cells]
	return([float(stat[5])/float(stat[4]), float(stat[6])/float(stat[4])])
# get percent of singles, doubles, triples, hrs, and walks that a pitcher gives up
def get_pitcher_percentages(team, pitcher):
	url = 'http://espn.go.com/mlb/team/stats/pitching/_/name/%s' % (team) 
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	table = soup.find(text = re.compile("NAME")).find_parent("table")
	rows = table.findAll('tr')
	player = table.find(text = re.compile(pitcher)).find_parent("tr")
	cells = player.findAll('td')
	# pull stat line 
	stat = [cell.text.strip('\n') for cell in cells]
	# rough calculation of batters faced (IP * 3) + hits + walks
	BF = (float(stat[8]) * 3) + float(stat[9]) + float(stat[12])
	# percent of home runs per batter faced
	P4 = float(stat[11])/BF
	pitcherValues = get_pitcher_doubles_triples()
	# percent of triples per batter faced
	P3 = (float(stat[9]) * pitcherValues[1])/BF
	# percent of doubles per batter faced
	P2 = (float(stat[9]) * pitcherValues[0])/BF
	# percent of singles per batter faced
	P1 = (float(stat[9])/BF) - P4 - P3 - P2
	# percent of walks per batter faced
	PBB = float(stat[12])/BF
	return(P1,P2,P3,P4,PBB)
# calculate league walk numbers (AL or NL) 
def get_league_BB(league):
	url = 'http://espn.go.com/mlb/stats/team/_/stat/batting/split/31/type/expanded'
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	# pull sortable batting table and get number of walks for the designated league
	table = soup.find(text = re.compile("Sortable Batting")).find_parent("table")
	rows = table.findAll('tr')
	if league == 'AL': 
		a = 'American League'
	elif league == 'NL':
		a = 'National League'
	x = table.find(text = re.compile(a)).find_parent("tr")
	stat = [i.text.strip('\n') for i in x]
	return(float(stat[2]))
# get league averages of singles per batter faced, double per batter faced, etc. 
def get_league_averages(league):
	url = 'http://espn.go.com/mlb/stats/team/_/stat/batting/split/31'
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	# pull sortable batting table and retrieve line of stats based of designated league
	table = soup.find(text = re.compile("Sortable Batting")).find_parent("table")
	rows = table.findAll('tr')
	if league == 'AL': 
		a = 'American League'
	elif league == 'NL':
		a = 'National League'
	x = table.find(text = re.compile(a)).find_parent("tr")
	stat = [i.text.strip('\n') for i in x]
	# pull number of batters faced and calculate number of singles per batter faced, etc. 
	BF = float(stat[2])
	LABB = get_league_BB(league)/BF
	LA1 = (float(stat[4])-float(stat[5])-float(stat[6])-float(stat[7]))/BF
	LA2 = float(stat[5])/BF
	LA3 = float(stat[6])/BF
	LA4 = float(stat[7])/BF
	return(LA1, LA2, LA3, LA4, LABB)
# log 5 formula used multiple times in get_probabilities
def log5(x,y,z):
	return(((x*y)/z)/(((x*y)/z) + ((1 -x)*(1-y))/(1-z)))
# main function in this python file -- gets probabilities of single, double, triple, home run, walk depending on the matchup, state, and count
def get_probabilities(batter_team, pitcher_team, batter, pitcher, league, state, count):
	# batter team abv is espn abbreviation for given mlb team
	batter_team_abv = batter_abv[batter_team]
	# pitcher team abv is yahoo abbreviation for given mlb team
	pitcher_team_abv = yahoo_abv[pitcher_team]
	# BP is the batting average for the batter dependent on the current state
	BP = get_ba_by_state(state, batter, batter_team_abv)
	# PP is the pitchers percentages of allowing a single, double, triple, home run, and walk
	PP = get_pitcher_percentages(pitcher_team_abv, pitcher)
	# if interleague game, take average of AL and NL averages
	if league == 'inter':
		data =[get_league_averages("AL"), get_league_averages("NL")]
		LA =[sum(e)/len(e) for e in zip(*data)]
	# if not interleague game, get league averages designated by given league
	else:
		LA = get_league_averages(league)
	# run log5 calculations with batters stats, pitchers stats, and league average stats for a given event
	psingle = log5(BP[0], PP[0], LA[0])
	pdouble = log5(BP[1], PP[1], LA[1])
	ptriple = log5(BP[2], PP[2], LA[2])
	phr = log5(BP[3], PP[3], LA[3])
	pbb = log5(BP[4], PP[4], LA[4])
	# using these probabilities, incorporate the difference for the current count
	pcSINGLE = get_count_probability(state, count, psingle) * 100
	pcDOUBLE = get_count_probability(state, count, pdouble) * 100 
	pcTRIPLE = get_count_probability(state, count, ptriple) * 100
	pcHR = get_count_probability(state, count, phr) * 100
	pcWALK = get_count_probability(state, count, pbb)* 100
	# return these probabilities rounded to the fourth decimal
	return(round(pcSINGLE,4), round(pcDOUBLE,4), round(pcTRIPLE,4), round(pcHR,4), round(pcWALK,4), round(100 - (pcSINGLE + pcDOUBLE + pcTRIPLE + pcHR + pcWALK), 4))