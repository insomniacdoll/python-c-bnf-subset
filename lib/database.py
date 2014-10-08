#!/usr/bin/env python

import copy
import MySQLdb.constants
import MySQLdb.converters
import MySQLdb.cursors
import itertools
import logging
import time

class Connection(object):
    def __init__(self, host, database, user=None, password=None,
                 max_idle_time=7*3600):
        self.host = host
        self.database = database
        self.max_idle_time = max_idle_time

        args = dict(conv=CONVERSIONS, use_unicode=True, charset="utf8",
                    db=database, init_command='SET time_zone = "+8:00"',
                    sql_mode="TRADITIONAL")
        if user is not None:
            args["user"] = user
        if password is not None:
            args["passwd"] = password

        if "/" in host:
            args["unix_socket"] = host
        else:
            self.socket = None
            pair = host.split(":")
            if len(pair) == 2:
                args["host"] = pair[0]
                args["port"] = int(pair[1])
            else:
                args["host"] = host
                args["port"] = 3306

        self._db = None
        self._db_args = args
        self._last_use_time = time.time()
        try:
            self.reconnect()
        except Exception:
            logging.error("Cannot connect to MySQL on %s", self.host,\
                          exc_info=True)

    def __del__(self):
        self.close()

    def close(self):
        if getattr(self, "_db", None) is not None:
            self._db.close()
            self._db = None

    def reconnect(self):
        self.close()
        try:
           from DBUtils import PooledDB

           pool_con = PooledDB.PooledDB(creator=MySQLdb, mincached=1, maxcached=10, maxshared=10,\
    maxconnections=20, blocking=False, maxusage=100, **self._db_args)
           self._db = pool_con.connection()
           self._db.cursor().connection.autocommit(True)
        except:
           self._db = MySQLdb.connect(**self._db_args)
           self._db.autocommit(True)

    def iter(self, query, *parameters):
        self._ensure_connected()
        cursor = MySQLdb.cursors.SSCursor(self._db)
        try:
            self._execute(cursor, query, parameters)
            column_names = [d[0] for d in cursor.description]
            for row in cursor:
                yield Row(zip(column_names, row))
        finally:
            cursor.close()

    def query(self, query, *parameters):
        cursor = self._cursor()
        try:
            self._execute(cursor, query, parameters)
            column_names = [d[0] for d in cursor.description]
            return [Row(itertools.izip(column_names, row)) for row in cursor]
        finally:
            cursor.close()

    def get(self, query, *parameters):
        rows = self.query(query, *parameters)
        if not rows:
            return None
        elif len(rows) > 1:
            raise Exception("Multiple rows returned for Database.get() query")
        else:
            return rows[0]

    def execute(self, query, *parameters):
        return self.execute_lastrowid(query, *parameters)

    def execute_lastrowid(self, query, *parameters):
        cursor = self._cursor()

        try:
            self._execute(cursor, query, parameters)
            return cursor.lastrowid
        finally:
            cursor.close()

    def execute_rowcount(self, query, *parameters):
        cursor = self._cursor()
        try:
            self._execute(cursor, query, parameters)
            return cursor.rowcount
        finally:
            cursor.close()

    def executemany(self, query, parameters):
        return self.executemany_lastrowid(query, parameters)

    def executemany_lastrowid(self, query, parameters):
        cursor = self._cursor()
        try:
            cursor.executemany(query, parameters)
            return cursor.lastrowid
        finally:
            cursor.close()

    def executemany_rowcount(self, query, parameters):
        cursor = self._cursor()
        try:
            cursor.executemany(query, parameters)
            return cursor.rowcount
        finally:
            cursor.close()

    def _ensure_connected(self):
        if (self._db is None or \
            (time.time() - self._last_use_time > self.max_idle_time)):
            self.reconnect()
        self._last_use_time = time.time()

    def _cursor(self):
        self._ensure_connected()
        return self._db.cursor()

    def _execute(self, cursor, query, parameters):
        try:
            return cursor.execute(query, parameters)
        except OperationalError:
            logging.error("Error connecting to MySQL on %s", self.host)
            self.close()
            raise
        finally:
            cursor.close()


class Row(dict):
    def __getattr__(self, name):
        try:
            return self[name]
        except KeyError:
            raise AttributeError(name)

FIELD_TYPE = MySQLdb.constants.FIELD_TYPE
FLAG = MySQLdb.constants.FLAG
CONVERSIONS = copy.copy(MySQLdb.converters.conversions)

field_types = [FIELD_TYPE.BLOB, FIELD_TYPE.STRING, FIELD_TYPE.VAR_STRING]
if 'VARCHAR' in vars(FIELD_TYPE):
    field_types.append(FIELD_TYPE.VARCHAR)

for field_type in field_types:
    CONVERSIONS[field_type] = [(FLAG.BINARY, str)] + CONVERSIONS[field_type]

IntegrityError = MySQLdb.IntegrityError
OperationalError = MySQLdb.OperationalError
