import csv
import itertools

def dimension_combinations(dimension_groups):
    list_of_combinations = []
    flat_dimensions = [[val for key,val in i.items()] for i in dimension_groups]

    for combination in flat_dimensions:
        combos = itertools.product(*[i for i in combination])
        for c in combos:
            list_of_combinations.append(c)
    return list_of_combinations

def getter_factory(key):
    def getter(l):
        t = [i[1] for i in l if i[0] == key]
        return t[0]
    return getter

get_town = getter_factory('town')
get_year = getter_factory('year')
get_race = getter_factory('race')
get_gender = getter_factory('gender')
get_age = getter_factory('age')
get_mt = getter_factory('measure type')
get_var = getter_factory('variable')

def build_lookup_key(d):
    town = get_town(d)
    year = get_year(d)
    race = get_race(d)
    gender = get_gender(d)
    age = get_age(d)
    mt = get_mt(d)
    var = get_var(d)
    return '{}_{}_{}_{}_{}_{}_{}'.format(town, year, race, gender, age, mt, var)


def build_key(d):
    town = d['Town']
    year = d['Year'].replace(' ', '')
    race = d['Race/Ethnicity']
    gender = d['Gender']
    age = d['Age Cohort']
    mt = d['Measure Type']
    var = d['Variable']
    return '{}_{}_{}_{}_{}_{}_{}'.format(town, year, race, gender, age, mt, var)


complete_lookup = {}
complete = []
with open('data/population-by-age-by-town.csv') as f:
    reader = csv.DictReader(f)
    for row in reader:
        complete.append(row)
        key = build_key(row)
        complete_lookup[key] = row


key_set = set([k for k,v in complete_lookup.items()])

towns = list({('town', c['Town']) for c in complete})
years = list({('year', c['Year']) for c in complete})
race = list({('race', c['Race/Ethnicity']) for c in complete})
age = list({('age', c['Age Cohort']) for c in complete})
gender = list({('gender', c['Gender']) for c in complete})
var = list({('variable', c['Variable']) for c in complete})
mt = list({('measure type', c['Measure Type']) for c in complete})

lookups = itertools.product(towns, years, race, age, gender, var, mt)
lookup_set = set([build_lookup_key(l) for l in lookups])

missing = lookup_set - key_set

missing_list = list(missing)

with open('raw/missing.csv', 'w') as f:
    writer = csv.writer(f, delimiter=',')
    for m in missing_list:
        row = m.split('_')
        writer.writerow(row)