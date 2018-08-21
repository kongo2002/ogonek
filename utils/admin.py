#!/usr/bin/env python

from __future__ import print_function

import sys

import requests


HOST = 'http://localhost:5984'
DBNAME = 'ogonek'


def __view_results(path):
    result = requests.get(path).json()
    rows = result.get('rows', [])
    return [x['value'] for x in rows]


def __delete_by_id(entity_id, revision):
    path = '%s/%s/%s' % (HOST, DBNAME, entity_id)
    headers = {'If-Match': revision}
    res = requests.delete(path, headers=headers)
    return res.status_code == 200


def __delete(obj):
    return __delete_by_id(obj['_id'], obj['_rev'])


def _buildings_of_planet(planet):
    arg = '"%s"' % planet
    req = '%s/%s/_design/building/_view/by_planet?key=%s' % (HOST, DBNAME, arg)
    return __view_results(req)


def _user_planets(user):
    arg = '"%s"' % user
    req = '%s/%s/_design/planet/_view/by_owner?key=%s' % (HOST, DBNAME, arg)
    planets = __view_results(req)

    for planet in planets:
        planet_id = planet['_id']
        planet['buildings'] = _buildings_of_planet(planet_id)

    return planets


if __name__ == '__main__':
    USER = sys.argv[1]
    PLANETS = _user_planets(USER)

    print(PLANETS)
