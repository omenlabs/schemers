#!/usr/bin/env python

#
# Attempt to take Chapter 20 and do a partial scheme in Python
#

import logging

LOGGER = logging.getLogger(__name__)

#
# Parse Functions
#
def parse_sexp(sexp):

    sexp += " " # get the last arg
    components = []
    current_comp = ""
    skip_space = -1

    for char in sexp:

        if char == '(':
            skip_space += 1
            #LOGGER.debug("Skip space: %s", skip_space)
        if char == ')':
            skip_space -= 1
            #LOGGER.debug("Skip space: %s", skip_space)

        if (skip_space == 0 and char in [' ', ')']):
            if char == ')':
                current_comp += char

            # Allow multiple spaces
            if current_comp:
                components.append(current_comp)
            current_comp = ""
        elif not (skip_space == 0 and char == '('):
            current_comp += char
    LOGGER.debug("Parsed sexp '%s' into '%s'", sexp, components)

    return components


def function_of(parsed):
    return parsed[0]

def arguments_of(parsed):
    return parsed[1:]

def identifier(e, table):
    return table[e]

#
# "Table" Functions
#
def quote(sexp, table):
    LOGGER.debug("Calling quote on '%s'", sexp)
    atom = parse_sexp(sexp)[1]
    if '(' in atom:
        res = atom.rstrip(')').lstrip('(').split()
    else:
        res = atom
    return res

#
# "Atomic" Functions
#
def cons(a, l):
    return l.append(a)

def null(a):
    return bool(a)

def car(l):
    return l[0]

def cdr(l):
    return l[1:]

def eq(a, b):
    return a == b

def atom(a):

    if ' ' in a or '(' in a:
        return False

    return True

def zero(a):

    try:
        int(a)
    except ValueError:
        return False
    else:
        return int(a) == 0

def add1(a):
    return int(a) + 1

def sub1(a):
    return int(a) - 1

def number(a):

    try:
        int(a)
    except ValueError:
        return False
    else:
        return True

#
# Function Lookup Routines
#
CONST = {'cons': cons,
         'car': car,
         'cdr': cdr,
         'null?': null,
         'eq?': eq,
         'atom?': atom,
         'zero?': zero,
         'add1': add1,
         'sub1': sub1,
         'number?': number,
         '#t': True,
         '#f': False
}

def const(e, table):
    # Are we a number?
    try:
        int(e)
    except ValueError:
        pass
    else:
        return int(e)

    # Else lookup
    if e in CONST:
        return CONST[e]

    raise Exception("Not a const")

def atom_to_action(e):
    LOGGER.debug("Calling atom_to_action on '%s'", e)
    if number(e):
        return const
    elif e in CONST:
        return const
    else:
        return identifier

def list_to_action(e):

    LOGGER.debug("Calling list_to_action on '%s'", e)
    TABLE_ACTIONS = {'quote': quote}

    parsed = parse_sexp(e)

    ee = parsed[0]

    if atom(ee):
        if ee in TABLE_ACTIONS:
            return TABLE_ACTIONS[ee]

    return application

def expression_to_action(e):

    if atom(e):
        return atom_to_action(e)
    else:
        return list_to_action(e)

def meaning(e, table):
    return expression_to_action(e)(e, table)

def evlist(args, table):

    LOGGER.debug("Calling evlist on %s", args)
    final_args = []

    for arg in args:
        final_args.append(meaning(arg, table))

    return final_args

def application(sexp, table):

    LOGGER.debug("Calling application on %s", sexp)

    parsed = parse_sexp(sexp)
    args = evlist(parsed[1:], table)

    return meaning(parsed[0], table)(*args)


def main():
    logging.basicConfig(level=logging.DEBUG)
    global_table = {}
    print application("(eq? 5 (add1 (add1 (add1 (car (cdr (quote (1 2 3))))))))", global_table)


if __name__ == "__main__":
    main()
