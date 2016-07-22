from bs4 import BeautifulSoup
import urllib2
import re
import csv

# script to scrape ESPN's MLB standings grid and write to csv file the NL standings
# input is current year as a string
def get_NL_standings(year):
    csv_filename = 'NL-standings.csv'
    url = 'http://espn.go.com/mlb/standings/grid/_/year/' + year
    page = urllib2.urlopen(url)
    soup = BeautifulSoup(page.read(), "html.parser")

    # Extracts the table for the NL and the rows for each team
    NL_table = soup.find(text = re.compile("National")).find_parent("table")
    NL_rows = NL_table.findAll('tr', class_ = re.compile("team"))
    # Creates a list of the NL teams and then appends AL for interleague games
    NL_teams = [team_row.find('b').text for team_row in NL_rows]
    NL_teams.append("AL")
    with open(csv_filename, 'wb') as f:
        csv_out = csv.writer(f)
        csv_out.writerow(['Team', 'Opponent', 'Wins', 'Losses'])        
        # For each team in the NL table, identifies the team's name, the opponent,
        # and their wins and losses (WL) against that opponent. Then outputs the results to the open CSV file
        for team_row in NL_rows:
            team = team_row.find('b').text
            WL_cells = team_row.findAll('td', align = "right")          
            # Extracts the values for both wins and losses from each WL table cell
            wins_losses = [td_cell.text.strip('\n').split('-') for td_cell in WL_cells]
            for i, opponent in enumerate(NL_teams):
                if team != opponent:
                    csv_out.writerow([team, opponent, wins_losses[i][0], wins_losses[i][1]])
    return('NL-standings.csv')