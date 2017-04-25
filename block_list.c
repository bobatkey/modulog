#include <stdlib.h>
#include <stdio.h>

#define BLOCK_SIZE 16

typedef struct _list_node list_node;

struct _list_node {
     int occupied;
     int values[BLOCK_SIZE];
     list_node *next;
};

void insert (list_node **head, int value)
{
     if (*head == NULL) {
          *head = malloc (sizeof(list_node));

          if (*head == NULL) {
               fprintf (stderr, "Failed to allocate memory for node");
               exit (EXIT_FAILURE);
          }

          (*head)->occupied = 1;
          (*head)->values[0] = value;
          (*head)->next = NULL;
     } else if ((*head)->occupied == BLOCK_SIZE) {
          list_node *new_head = malloc (sizeof(list_node));

          if (new_head == NULL) {
               fprintf (stderr, "Failed to allocate memory for node");
               exit (EXIT_FAILURE);
          }

          new_head->occupied = 1;
          new_head->values[0] = value;
          new_head->next = *head;

          *head = new_head;
     } else {
          (*head)->values[(*head)->occupied] = value;
          (*head)->occupied++;
     }
}

void iterate (list_node *node)
{
     while (node != NULL) {
          for (int i = 0; i < node->occupied; i++)
               printf ("%d\n", node->values[i]);

          node = node->next;
     }
}
