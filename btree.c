/* Lets write a btree! Keys will be ints. */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define MIN_CHILDREN 15

typedef struct _node node;

struct _node {
     bool leaf;
     int nkeys;
     int key[2*MIN_CHILDREN-1];
     node* children[2*MIN_CHILDREN];
};

void print_tree (node * node, int indent)
{
     if (node->leaf) {
          for (int i = 0; i < indent*2; i++) putchar (' ');
          putchar ('[');
          for (int i = 0; i < node->nkeys; i++) printf (" %d", node->key[i]);
          printf (" ]\n");
     } else {
          for (int i = 0; i < indent*2; i++) putchar (' ');
          printf ("[\n");
          for (int i = 0; i <= node->nkeys; i++) {
               print_tree (node->children[i], indent+1);
               if (i != node->nkeys) {
                    for (int i = 0; i < indent*2; i++) putchar (' ');
                    printf ("%d\n", node->key[i]);
               }
          }
          for (int i = 0; i < indent*2; i++) putchar (' ');
          printf ("]\n");
     }
}


node* create()
{
     node *x;

     x = malloc (sizeof(node));
     if (x == NULL) {
          fprintf (stderr, "Failed to allocate a new node");
          exit (EXIT_FAILURE);
     }

     x->leaf = true;
     x->nkeys = 0;

     return x;
}


bool member(int key, node *x)
{
     int i;

     while (true) {
          i = 0;

          while (i < x->nkeys && x->key[i] < key)
               i++;

          if (i < x->nkeys && x->key[i] == key)
               return true;

          if (x->leaf)
               return false;

          x = x->children[i];
     }
}

void iterate_range (int from, int to, node *x)
{
     node *stack[20];
     int stack_child[20];
     int stackptr = 0;
     int i;

     /* step 1: find 'from', or first key larger */

     while (true) {
          for (i = 0; i < x->nkeys && x->key[i] < from; i++);

          if (x->leaf) break;

          if (i < x->nkeys) {
               stack[stackptr] = x;
               stack_child[stackptr] = i;
               stackptr++;
          }

          x = x->children[i];
     }

     /* We are now at a leaf node at the first key >= from; scan
      * through the keys until we hit the first key that is >= to. */

     while (true) {
          for (; i < x->nkeys && x->key[i] < to; i++)
               printf ("%d ", x->key[i]);

          if (i != x->nkeys || stackptr == 0)
               break;

          x = stack[stackptr-1];
          i = stack_child[stackptr-1];

          if (!(x->key[i] < to))
               break;

          printf ("%d ", x->key[i]);

          if (i == x->nkeys-1)
               stackptr--;
          else
               stack_child[stackptr-1] = i + 1;

          x = x->children[i+1];
          while (!x->leaf) {
               //printf ("pushing\n");
               stack[stackptr] = x;
               stack_child[stackptr] = 0;
               stackptr++;
               x = x->children[0];
          }

          i = 0;
     }
}

void iterate_all (node *x)
{
     node *stack[20];
     int stack_child[20];
     int stackptr = 0;
     int i;

     while (!x->leaf) {
          stack[stackptr] = x;
          stack_child[stackptr] = 0;
          stackptr++;
          x = x->children[0];
     }

     while (true) {
          for (i = 0; i < x->nkeys; i++)
               printf ("%d ", x->key[i]);

          if (stackptr == 0)
               break;

          x = stack[stackptr-1];
          i = stack_child[stackptr-1];

          printf ("%d ", x->key[i]);

          if (i == x->nkeys-1)
               stackptr--;
          else
               stack_child[stackptr-1] = i + 1;

          x = x->children[i+1];

          while (!x->leaf) {
               stack[stackptr] = x;
               stack_child[stackptr] = 0;
               stackptr++;
               x = x->children[0];
          }
     }
}

void split_child (node *x, int i, node *y)
{
     node *z = malloc (sizeof(node));
     if (z == NULL) {
          fprintf (stderr, "Failed to allocate node\n");
          exit(EXIT_FAILURE);
     }

     z->leaf = y->leaf;

     z->nkeys = MIN_CHILDREN - 1;

     for (int j = 0; j < MIN_CHILDREN - 1; j++)
          z->key[j] = y->key[j+MIN_CHILDREN];

     if (!y->leaf)
          for (int j = 0; j < MIN_CHILDREN; j++)
               z->children[j] = y->children[j+MIN_CHILDREN];

     y->nkeys = MIN_CHILDREN - 1;

     for (int j = x->nkeys; j > i; j--)
          x->children[j+1] = x->children[j];

     x->children[i+1] = z;

     for (int j = x->nkeys - 1; j >= i; j--)
          x->key[j+1] = x->key[j];

     x->key[i] = y->key[MIN_CHILDREN-1];

     x->nkeys++;
}


void insert_nonfull (node *x, int key)
{
     int i;

     while (!x->leaf) {
          i = x->nkeys - 1;

          while (i >= 0 && key < x->key[i])
               i--;

          i++;

          if (x->children[i]->nkeys == 2*MIN_CHILDREN - 1) {
               split_child (x, i, x->children[i]);
               if (x->key[i] < key)
                    i++;
          }

          x = x->children[i];
     }

     /* found the leaf node for insertion */

     i = x->nkeys - 1;
     while (i >= 0 && key < x->key[i]) {
          x->key[i+1] = x->key[i];
          i--;
     }

     x->key[i+1] = key;

     x->nkeys++;
}


void insert(int key, node **root)
{
     node * x;

     x = *root;

     if (x->nkeys == 2*MIN_CHILDREN - 1) {
          node * s;

          s = malloc (sizeof(node));
          if (s == NULL) {
               fprintf(stderr, "Unable to allocate new node");
               exit (EXIT_FAILURE);
          }

          *root = s;

          s->leaf = false;
          s->nkeys = 0;
          s->children[0] = x;

          split_child (s, 0, x);
          insert_nonfull (s, key);
     } else {
          insert_nonfull (x, key);
     }
}

int main(int argc, char* argv[])
{
     printf ("sizeof(node) = %zd\n", sizeof(node));

     node * tree;

     tree = create ();

     int N = 10000;

     for (int i = 0; i < N/2; i++) {
//          printf ("Attempting to insert %d\n", i*2);
          insert (i*2, &tree);
//          print_tree (tree, 0);
//          printf ("\n");
     }

     //print_tree (tree, 0);

     iterate_range (17, 34, tree);
     printf ("\n");

     for (int i = 0; i < N; i++) {
          bool result = member (i, tree);
          if (i % 2 == 0 && !result) {
               fprintf (stderr, "Failed to find %d in intermediate tree\n", i);
               exit (EXIT_FAILURE);
          }
     }

     for (int i = 0; i < N/2; i++)
          insert (i*2+1, &tree);

     iterate_range (17, 34, tree);
     printf ("\n");

//     print_tree (tree, 0);

//     iterate_all (tree);
//     printf ("\n");
     
     for (int i = 0; i < N; i++) {
          if (!member(i, tree)) {
               fprintf (stderr, "Failed to find %d in final tree\n", i);
               exit (EXIT_FAILURE);
          }
     }
}
