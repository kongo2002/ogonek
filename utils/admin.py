#!/usr/bin/env python

from __future__ import print_function

import json
import sys

import requests


HOST = 'http://localhost:5984'
DBNAME = 'ogonek'


def __pretty(obj):
    print(json.dumps(obj))


def __view_results(path):
    result = requests.get(path).json()
    rows = result.get('rows', [])
    return [x['value'] for x in rows]


def __view_result(path):
    results = __view_results(path)
    if results:
        return results[0]
    return None


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


def _local_user(email):
    arg = '"%s"' % email
    req = '%s/%s/_design/user/_view/local?key=%s' % (HOST, DBNAME, arg)
    return __view_result(req)


def _local_users():
    req = '%s/%s/_design/user/_view/local' % (HOST, DBNAME)
    return [x['_id'] for x in __view_results(req)]


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('usage: %s <operation> [<arg>...]' % sys.argv[0], file=sys.stderr)
        sys.exit(1)

    OP = sys.argv[1]

    if OP == 'planets':
        if len(sys.argv) > 2:
            USER = sys.argv[2]
            PLANETS = _user_planets(USER)
            __pretty(PLANETS)
        else:
            print('usage: %s planets <user>' % sys.argv[0], file=sys.stderr)
            sys.exit(1)
    elif OP == 'local':
        if len(sys.argv) > 2:
            EMAIL = sys.argv[2]
            __pretty(_local_user(EMAIL))
        else:
            __pretty(_local_users())
    else:
        print('unknown operation "%s"' % OP)
        sys.exit(2)
