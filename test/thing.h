//	Mohammad Faiz ATHER === UQ 2020
//
//	thing.h
//	an ADT for a primitive int Thing
//
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#ifndef THING_H_
#define THING_H_

typedef int Thing;

static inline Thing
thing_copy (Thing t)
{
	return t;
}

// remember to free pointer
static inline char *
thing_show (Thing t)
{
	char *show = (char *)malloc (sizeof (char) * 100);
	snprintf(show, 100, "%d", t);
	return show;
}

// remember to free pointer
static inline void
thing_show_destroy (char *s)
{
	free (s);
}

static inline size_t
thing_size (Thing t)
{
	return sizeof(t);
}

static inline Thing
thing_null (void)
{
	return 0;
}

static inline void
thing_destroy (Thing t __attribute__((unused)))
{
	return;
}

static inline bool
thing_is (char *c)
{
	bool is = true;
	for (int i = 0; *(c + i) != '\0'; i++)
	{
		if ( i == 0 && *(c + i) == '-' )
		{
			continue;
		} else {
			if ( *(c + i) < '0' || *(c + i) > '9' ) is = false;
		}
	}
	return is;
} 

#define Thing Thing

#endif
