//	Mohammad Faiz ATHER === UQ 2020
//
//	summex.c
//	tute		1
//	exercise	1
//
#include "stack.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

Stack
parse_args (int size, char **stream);

int
main(int argc, char **argv)
{
	Stack s = parse_args (argc, argv);
	int big = (1 >> (sizeof(int)*8-1));
	int curr = 0;
	int sum = 0;
	while (stack_size (s) > 0)
	{
		curr = (int)stack_pop (s);
		if (curr > big) big = curr;
		sum += curr;
	}

	char *s1 = thing_show (sum);
	char *s2 = thing_show (big);
	printf("%s %s\n", s1, s2);
	thing_show_destroy (s1);
	thing_show_destroy (s2);

	stack_destroy (s);

	return 0;
}

Stack
parse_args (int size, char **stream)
{
	Stack s = stack_init ();
	for (int i = 1; i < size; i++)
	{
		if (thing_is (stream[i]) == false)
		{
			stack_destroy (s);
			exit (1);
		}
		stack_push (s, (Thing)atoi (stream[i]));
	}
	return s;
}
