import csv
from bs4 import BeautifulSoup
import urllib2
import re

states = []
runsexp = []
with open('1990.2015.RE.States.Count.csv', 'rb') as file:
	counts = csv.reader(file, delimiter = ',')
	for row in counts:
		states.append(str(row[0]))
		runsexp.append(row[1:])
count_state = dict(zip(states, runsexp))

yahoo_abv = {'Angels':'laa', 'Orioles':'bal', 'Red Sox':'bos', 'White Sox':'chw', 'Indians':'cle', 'Tigers':'det', 'Astros':'hou', 'Royals':'kan', 'Twins':'min', 'Yankees':'nyy', 'Athletics':'oak', 'Mariners':'sea', 'Rays':'tam', 'Rangers':'tex', 'Blue Jays':'tor', 'Diamondbacks':'ari', 'Braves':'atl', 'Cubs':'chc', 'Reds':'cin', 'Rockies':'col', 'Dodgers':'lad', 'Marlins':'mia', 'Brewers':'mil', 'Mets':'nym', 'Phillies':'phi', 'Pirates':'pit', 'Padres':'sdg', 'Giants':'sfo', 'Cardinals':'stl', 'Nationals':'was'}
batter_abv ={'Angels':'laa', 'Orioles':'bal', 'Red Sox':'bos', 'White Sox':'chw', 'Indians':'cle', 'Tigers':'det', 'Astros':'hou', 'Royals':'kc', 'Twins':'min', 'Yankees':'nyy', 'Athletics':'oak', 'Mariners':'sea', 'Rays':'tb', 'Rangers':'tex', 'Blue Jays':'tor', 'Diamondbacks':'ari', 'Braves':'atl', 'Cubs':'chc', 'Reds':'cin', 'Rockies':'col', 'Dodgers':'la', 'Marlins':'mia', 'Brewers':'mil', 'Mets':'nym', 'Phillies':'phi', 'Pirates':'pit', 'Padres':'sd', 'Giants':'sf', 'Cardinals':'stl', 'Nationals':'was'}

AL = ['Angels','Orioles','Red Sox','White Sox','Indians','Tigers','Astros','Royals','Twins','Yankees','Athletics','Mariners','Rays','Rangers','Blue Jays']
NL = ['Diamondbacks','Braves','Cubs','Reds','Rockies','Dodgers','Marlins','Brewers','Mets','Phillies','Pirates','Padres','Giants','Cardinals','Nationals']

# def get_batter_percentages(player, team):
# 	url = 'http://espn.go.com/mlb/team/stats/batting/_/name/%s' % (team) 
# 	page = urllib2.urlopen(url)
# 	soup = BeautifulSoup(page.read(), "html.parser")
# 	table = soup.find(text = re.compile("NAME")).find_parent("table")
# 	rows = table.findAll('tr')
# 	try:
# 		player = table.find(text = re.compile(player)).find_parent("tr")
# 		cells = player.findAll('td')
# 		stat = [cell.text.strip('\n') for cell in cells]
# 		# stat is line of player's stats
# 		# calculations below find percents of singles/hits, doubles/hits, etc
# 		single = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
# 		print(single)
# 		psingle = single/(float(stat[2])+float(stat[10]))
# 		pdouble = float(stat[5])/(float(stat[2])+float(stat[10]))
# 		ptriple = float(stat[6])/(float(stat[2])+float(stat[10]))
# 		phr = float(stat[7])/(float(stat[2])+float(stat[10]))
# 		pWALK = float(stat[10])/(float(stat[2])+float(stat[10]))
# 		if psingle == 0 and pdouble == 0 and ptriple == 0 and phr == 0 and pWALK == 0:
# 			player = table.find(text = re.compile('Totals')).find_parent("tr")
# 			cells = player.findAll('td')
# 			stat = [cell.text.strip('\n') for cell in cells]
# 			# stat is line of player's stats
# 			# calculations below find percents of singles/hits, doubles/hits, etc
# 			single = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
# 			psingle = single/(float(stat[2])+float(stat[10]))
# 			pdouble = float(stat[5])/(float(stat[2])+float(stat[10]))
# 			ptriple = float(stat[6])/(float(stat[2])+float(stat[10]))
# 			phr = float(stat[7])/(float(stat[2])+float(stat[10]))
# 			pWALK = float(stat[10])/(float(stat[2])+float(stat[10]))
# 			return(psingle,pdouble,ptriple,phr,pWALK)
# 		else:
# 			return(psingle,pdouble,ptriple,phr,pWALK)
# 	except:
# 		player = table.find(text = re.compile('Totals')).find_parent("tr")
# 		cells = player.findAll('td')
# 		stat = [cell.text.strip('\n') for cell in cells]
# 		# stat is line of player's stats
# 		# calculations below find percents of singles/hits, doubles/hits, etc
# 		single = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
# 		psingle = single/(float(stat[2])+float(stat[10]))
# 		pdouble = float(stat[5])/(float(stat[2])+float(stat[10]))
# 		ptriple = float(stat[6])/(float(stat[2])+float(stat[10]))
# 		phr = float(stat[7])/(float(stat[2])+float(stat[10]))
# 		pWALK = float(stat[10])/(float(stat[2])+float(stat[10]))
# 		return(psingle,pdouble,ptriple,phr,pWALK)

def get_count_probability(state, count, probability):
	index = count_state[''].index(count)
	runs = count_state[state][index]
	base = count_state[state][0]
	delta = float(runs) - float(base)
	if delta > 0:
		return((1+(delta/10)) * probability)
	elif delta < 0:
		x = (1+abs(delta/10)) * probability
		dif = x - probability
		return(probability - dif)

def get_ba_runners_on(state, batter, batter_team):
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
		# scoring position
		url = 'http://espn.go.com/mlb/team/stats/batting/_/name/%s/split/39' % (batter_team) 
	elif state in ['110 2', '101 2', '011 2', '010 2', '001 2']:
		# scoring position 2 outs
		url = 'http://espn.go.com/mlb/team/stats/batting/_/name/%s/split/185' % (batter_team) 
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	table = soup.find(text = re.compile("NAME")).find_parent("table")
	rows = table.findAll('tr')
	try:
		player = table.find(text = re.compile(batter)).find_parent("tr")
		cells = player.findAll('td')
		stat = [cell.text.strip('\n') for cell in cells]
		# stat is line of player's stats
		# calculations below find percents of singles/hits, doubles/hits, etc
		single = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
		psingle = single/(float(stat[2])+float(stat[10]))
		pdouble = float(stat[5])/(float(stat[2])+float(stat[10]))
		ptriple = float(stat[6])/(float(stat[2])+float(stat[10]))
		phr = float(stat[7])/(float(stat[2])+float(stat[10]))
		pWALK = float(stat[10])/(float(stat[2])+float(stat[10]))
		if float(stat[2]) < 100:
			player = table.find(text = re.compile('Totals')).find_parent("tr")
			cells = player.findAll('td')
			stat = [cell.text.strip('\n') for cell in cells]
			# stat is line of player's stats
			# calculations below find percents of singles/hits, doubles/hits, etc
			single2 = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
			psingle2 = single2/(float(stat[2])+float(stat[10]))
			pdouble2 = float(stat[5])/(float(stat[2])+float(stat[10]))
			ptriple2 = float(stat[6])/(float(stat[2])+float(stat[10]))
			phr2 = float(stat[7])/(float(stat[2])+float(stat[10]))
			pWALK2 = float(stat[10])/(float(stat[2])+float(stat[10]))
			return((psingle + psingle2)/2, (pdouble + pdouble2)/2, (ptriple + ptriple2)/2, (phr + phr2)/2, (pWALK + pWALK2/2))
		if psingle == 0 and pdouble == 0 and ptriple == 0 and phr == 0 and pWALK == 0:
			player = table.find(text = re.compile('Totals')).find_parent("tr")
			cells = player.findAll('td')
			stat = [cell.text.strip('\n') for cell in cells]
			# stat is line of player's stats
			# calculations below find percents of singles/hits, doubles/hits, etc
			single = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
			psingle = single/(float(stat[2])+float(stat[10]))
			pdouble = float(stat[5])/(float(stat[2])+float(stat[10]))
			ptriple = float(stat[6])/(float(stat[2])+float(stat[10]))
			phr = float(stat[7])/(float(stat[2])+float(stat[10]))
			pWALK = float(stat[10])/(float(stat[2])+float(stat[10]))
			return(psingle,pdouble,ptriple,phr,pWALK)
		else:
			return(psingle,pdouble,ptriple,phr,pWALK)
	except:
		player = table.find(text = re.compile('Totals')).find_parent("tr")
		cells = player.findAll('td')
		stat = [cell.text.strip('\n') for cell in cells]
		# stat is line of player's stats
		# calculations below find percents of singles/hits, doubles/hits, etc
		single = float(stat[4]) - (float(stat[5]) + float(stat[6]) + float(stat[7]))
		psingle = single/(float(stat[2])+float(stat[10]))
		pdouble = float(stat[5])/(float(stat[2])+float(stat[10]))
		ptriple = float(stat[6])/(float(stat[2])+float(stat[10]))
		phr = float(stat[7])/(float(stat[2])+float(stat[10]))
		pWALK = float(stat[10])/(float(stat[2])+float(stat[10]))
		return(psingle,pdouble,ptriple,phr,pWALK)

def get_pitcher_percentages(team, pitcher):
	url = 'http://espn.go.com/mlb/team/stats/pitching/_/name/%s' % (team) 
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	table = soup.find(text = re.compile("NAME")).find_parent("table")
	rows = table.findAll('tr')
	player = table.find(text = re.compile(pitcher)).find_parent("tr")
	cells = player.findAll('td')
	# stat is stat line of all players stats
	stat = [cell.text.strip('\n') for cell in cells]
	# stat[13] is player's batting average
	BF = (float(stat[8]) * 3) + float(stat[9]) + float(stat[12])
	P4 = float(stat[11])/BF
	P3 = (float(stat[9]) * 0.024)/BF
	P2 = (float(stat[9]) * 0.174)/BF
	P1 = (float(stat[9])/BF) - P4 - P3 - P2
	PBB = float(stat[12])/BF
	return(P1,P2,P3,P4,PBB)

def get_league_BB(league):
	url = 'http://espn.go.com/mlb/stats/team/_/stat/batting/split/31/type/expanded'
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	table = soup.find(text = re.compile("Sortable Batting")).find_parent("table")
	rows = table.findAll('tr')
	if league == 'AL': 
		a = 'American League'
	elif league == 'NL':
		a = 'National League'
	x = table.find(text = re.compile(a)).find_parent("tr")
	stat = [i.text.strip('\n') for i in x]
	return(float(stat[2]))

def get_league_averages(league):
	url = 'http://espn.go.com/mlb/stats/team/_/stat/batting/split/31'
	page = urllib2.urlopen(url)
	soup = BeautifulSoup(page.read(), "html.parser")
	table = soup.find(text = re.compile("Sortable Batting")).find_parent("table")
	rows = table.findAll('tr')
	if league == 'AL': 
		a = 'American League'
	elif league == 'NL':
		a = 'National League'
	x = table.find(text = re.compile(a)).find_parent("tr")
	stat = [i.text.strip('\n') for i in x]
	BF = float(stat[2])
	LABB = get_league_BB(league)/BF
	LA1 = (float(stat[4])-float(stat[5])-float(stat[6])-float(stat[7]))/BF
	LA2 = float(stat[5])/BF
	LA3 = float(stat[6])/BF
	LA4 = float(stat[7])/BF
	return(LA1, LA2, LA3, LA4, LABB)

def log5(x,y,z):
	return(((x*y)/z)/(((x*y)/z) + ((1 -x)*(1-y))/(1-z)))
		
def get_probabilities(batter_team, pitcher_team, batter, pitcher, league, state, count):
	batter_team_abv = batter_abv[batter_team]
	pitcher_team_abv = yahoo_abv[pitcher_team]
	BP = get_ba_runners_on(state, batter, batter_team_abv)
	PP = get_pitcher_percentages(pitcher_team_abv, pitcher)
	if league == 'inter':
		data =[get_league_averages("AL"), get_league_averages("NL")]
		LA =[sum(e)/len(e) for e in zip(*data)]
	else:
		LA = get_league_averages(league)
	psingle = log5(BP[0], PP[0], LA[0])
	pdouble = log5(BP[1], PP[1], LA[1])
	ptriple = log5(BP[2], PP[2], LA[2])
	phr = log5(BP[3], PP[3], LA[3])
	pbb = log5(BP[4], PP[4], LA[4])
	pcSINGLE = get_count_probability(state, count, psingle) * 100
	pcDOUBLE = get_count_probability(state, count, pdouble) * 100 
	pcTRIPLE = get_count_probability(state, count, ptriple) * 100
	pcHR = get_count_probability(state, count, phr) * 100
	pcWALK = get_count_probability(state, count, pbb)* 100
	return(round(pcSINGLE,4), round(pcDOUBLE,4), round(pcTRIPLE,4), round(pcHR,4), round(pcWALK,4), round(100 - (pcSINGLE + pcDOUBLE + pcTRIPLE + pcHR + pcWALK), 4))


# print(get_probabilities('Red Sox', 'Dodgers', 'Brock Holt', 'Brock Stewart', 'inter', '111 0', 'c00'))
# print(get_probabilities('Red Sox', 'Dodgers', 'Mookie Betts', 'Brock Stewart', 'inter', '111 2', 'c00'))
