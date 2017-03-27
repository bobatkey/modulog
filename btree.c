/* Lets write a btree! Keys will be ints. */

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdint.h>
#include <stddef.h>

struct mykey {
     uint32_t a1;
     uint32_t a2;
     uint32_t a3;
     uint32_t a4;
};

int mykey_cmp (struct mykey x, struct mykey y)
{
     return
          x.a1 != y.a1 ? (x.a1 < y.a1 ? -1 : 1) :
          x.a2 != y.a2 ? (x.a2 < y.a2 ? -1 : 1) :
          x.a3 != y.a3 ? (x.a3 < y.a3 ? -1 : 1) :
          x.a4 != y.a4 ? (x.a4 < y.a4 ? -1 : 1) :
          0;
}

int mykey_eq (struct mykey x, struct mykey y)
{
     return x.a1 == y.a1 && x.a2 == y.a2 && x.a3 == y.a3 && x.a4 == y.a4;
}

int mykey_lt (struct mykey x, struct mykey y)
{
     return
          x.a1 < y.a1
          || (x.a1 == y.a1
              && (x.a2 < y.a2
                  || (x.a2 == y.a2
                      && (x.a3 < y.a3
                          || (x.a3 == y.a3
                              && x.a4 < y.a4)))));
}



#define MIN_CHILDREN 8

typedef struct _node node;

struct _node {
     bool leaf;
     short nkeys;
     int key[2*MIN_CHILDREN-1];
     node *children[0];
};

node *allocate_node (bool leaf)
{
     size_t size = sizeof(node) + (leaf?0:sizeof(node*)*2*MIN_CHILDREN);
     node *x = malloc (size);
     if (x == NULL) {
          fprintf (stderr, "Failed to allocate node");
          exit (EXIT_FAILURE);
     }

     x->leaf = leaf;

     return x;
}

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
     node *x = allocate_node (true);

     x->nkeys = 0;

     return x;
}


/* this seems to slow things down at the moment, but might be faster
 * if key comparisons become more expensive. */
int bin_search (int *keys, int lo, int hi, int key)
{
     while (lo <= hi) {
          int mid = (hi + lo)/2;
          if (keys[mid] == key)
               return mid;
          else if (keys[mid] > key)
               hi = mid-1;
          else
               lo = mid+1;
     }
     return hi+1;
}


bool member(int key, node *x)
{
     int i;

     while (true) {
          for (i = 0; i < x->nkeys && x->key[i] < key; i++);

          if (i < x->nkeys && x->key[i] == key)
               return true;

          if (x->leaf)
               return false;

          x = x->children[i];
     }
}

bool member_range (int from, int to, node *x)
{
     int i;

     while (true) {
          for (i = 0; i < x->nkeys && x->key[i] < from; i++);

          if (i < x->nkeys && x->key[i] <= to)
               return true;

          if (x->leaf)
               return false;

          x = x->children[i];
     }
}

/* iterate_range (from, to, x) prints out all the keys stored in [x]
 * between [from] and [to], inclusive.
 */
void iterate_range (int from, int to, node *x)
{
     node *stack[30];
     int stack_child[30];
     int stackptr = 0;
     int i;

     /* FIXME: could also store a parent pointer/index in each node
      * (except the root). At a cost of one pointer and a small
      * integer per node. But then the parent index/pointers would
      * need to be updated during split child.
      *
      * Alternatively, use a B+-tree. When inserting, push the keys
      * down to the leaves. This means that, when splitting in
      * split_child, we don't take the key at MIN_CHILDREN-1 out of
      * the left child.
      */

     /* Find 'from', or first key larger */

     while (true) {
          for (i = 0; i < x->nkeys && x->key[i] < from; i++);

          if (x->leaf) break;

          /* only push a stack frame if we are not at the last
           * position -- tail recursion optimisation */
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
          for (; i < x->nkeys && x->key[i] <= to; i++)
               printf ("%d ", x->key[i]);

          if (i != x->nkeys || stackptr == 0)
               break;

          x = stack[stackptr-1];
          i = stack_child[stackptr-1];

          if (!(x->key[i] <= to))
               break;

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

void split_child (node *x, int i)
{
     node *y = x->children[i];
     node *z = allocate_node (y->leaf);

     z->nkeys = MIN_CHILDREN - 1;

     /*
     for (int j = 0; j < MIN_CHILDREN - 1; j++)
          z->key[j] = y->key[j+MIN_CHILDREN];
     */
     memcpy (&(z->key[0]), &(y->key[MIN_CHILDREN]), (MIN_CHILDREN-1) * sizeof(int));

     if (!y->leaf)
          memcpy (&(z->children[0]), &(y->children[MIN_CHILDREN]), MIN_CHILDREN * sizeof(node*));
//          for (int j = 0; j < MIN_CHILDREN; j++)
//               z->children[j] = y->children[j+MIN_CHILDREN];

     /* FIXME: for B+-trees, if y is a leaf, set y->nkeys =
      * MIN_CHILDREN to keep the middle node in the leaf. Then also
      * update the next pointer (i.e. children[0]). */
     y->nkeys = MIN_CHILDREN - 1;

     memmove (&(x->children[i+2]), &(x->children[i+1]), (x->nkeys - i) * sizeof(node*));
     /* for (int j = x->nkeys; j > i; j--) { */
     /*      printf ("Moving child %d -> %d\n",j,j+1);  */
     /*      x->children[j+1] = x->children[j]; */
     /* } */

     memmove (&(x->key[i+1]), &(x->key[i]), (x->nkeys - i) * sizeof(int));
     /* for (int j = x->nkeys - 1; j >= i; j--) */
     /*      x->key[j+1] = x->key[j]; */

     x->children[i+1] = z;
     x->key[i] = y->key[MIN_CHILDREN-1];
     x->nkeys++;
}

void insert_nonfull (node *x, int key)
{
     int i;

     while (!x->leaf) {
          for (i = 0; i < x->nkeys && x->key[i] < key; i++);

          if (x->children[i]->nkeys == 2*MIN_CHILDREN-1) {
               split_child (x, i);
               if (x->key[i] < key)
                    i++;
          }

          x = x->children[i];
     }

     /* found the leaf node for insertion */

     /* for (i = 0; i < x->nkeys && x->key[i] < key; i++); */
     /* memmove (&(x->key[i+1]), &(x->key[i]), (x->nkeys - i) * sizeof(int)); */
     /* x->key[i] = key; */

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
     if ((*root)->nkeys == 2*MIN_CHILDREN-1) {
          node *s = allocate_node (false);

          s->nkeys = 0;
          s->children[0] = *root;

          split_child (s, 0);

          *root = s;
     }

     insert_nonfull (*root, key);
}

/**********************************************************************/
/* the other kind of insertion: rebuild the tree as we go back up */

void insert2 (int key, node **root)
{
     /* 1. search to find the leaf node to insert into */
     /* 2. if the node is now full, then split it in its parent and
      * insert the new item at the end of the left hand one. */
     /* carry on splitting as we go back up the stack */

     /* if we are doing a bulk load in sorted order, we can maintain
      * the stack in between insertions, and use it to restart the
      * insertion process. We'll have to restart after a split, but
      * usually we'll be saving time. */

     node* stack[30];
     int stack_child[30];
     int stackptr = 0;

     int i;

     node *x = *root;

     /* search down the tree, remembering the nodes we passed. */
     while (!x->leaf) {
          for (i = 0; i < x->nkeys && x->key[i] < key; i++);

          stack[stackptr] = x;
          stack_child[stackptr] = i;
          stackptr++;

          x = x->children[i];
     }

     if (x->nkeys == 2*MIN_CHILDREN-1) {
          /* leaf is full, need to split. */

          /* make a new leaf node and copy over the top half of the
           * elements, using the new middle element as the median. */
          /* insert the new element into the appropriate side. */
          


          /* now unwind the stack, with the knowledge that we need to insert a
           * new key and trailing child. If the next one up the stack
           * becomes too full, then do the same splitting */
          while (stackptr != 0) {
               node *y = stack[stackptr-1];
               i = stack_child[stackptr-1];
               stackptr--;

               if (y->nkeys == 2*MIN_CHILDREN-1) {
                    /* split this node */
                    
               } else {
                    /* shunt the other keys and children up */
                    /* insert median key and new child here */
                    break;
               }
          }

     } else {
          /* leaf has space: insert here */
          i = x->nkeys - 1;
          while (i >= 0 && key < x->key[i]) {
               x->key[i+1] = x->key[i];
               i--;
          }

          x->key[i+1] = key;
          x->nkeys++;

          /* if we keep the stack here, then we won't have to recurse
           * back down if we want to insert a new key that is slightly
           * larger than this one. Need to write some code that can
           * advance from one key to the next (like the iterate
           * code). */
     }
}

/**********************************************************************/
/* bulk loading:
 * 
 * if we have a sorted list of items to insert, then repeatedly doing
 * insertions using 'insert' will be slow.
 * 
 * after doing an insertion, we know 
 */

int main(int argc, char* argv[])
{
     printf ("sizeof(node) = %zd\n", sizeof(node));
     printf ("offsetof(struct _node, leaf) = %zd\n", offsetof(struct _node, leaf));
     printf ("offsetof(struct _node, nkeys) = %zd\n", offsetof(struct _node, nkeys));
     printf ("offsetof(struct _node, key) = %zd\n", offsetof(struct _node, key));
     printf ("offsetof(struct _node, children) = %zd\n", offsetof(struct _node, children));

     node *tree = create ();

     int N = 100000000;

//     for (int i = N/2-1; i >= 0; i--)
     for (int i = 0; i < N/2; i++)
          insert (i*2, &tree);

     //print_tree (tree, 0);

     iterate_range (17, 17, tree);
     printf ("\n");

     iterate_range (17, 35, tree);
     printf ("\n");

     for (int i = 0; i < N; i++) {
          bool result = member (i, tree);
          if ((i % 2 == 0 && !result) || (i % 2 == 1 && result)) {
               fprintf (stderr, "Failed to find %d in intermediate tree\n", i);
               exit (EXIT_FAILURE);
          }
     }

     if (member_range (14, 14, tree))
          printf ("tree contains elements in range 14-14\n");
     else
          printf ("tree does not contain elements in range 14-14\n");
     if (member_range (15, 18, tree))
          printf ("tree contains elements in range 15-18\n");
     else
          printf ("tree does not contain elements in range 15-18\n");
     if (member_range (15, 15, tree))
          printf ("tree contains elements in range 15-15\n");
     else
          printf ("tree does not contain elements in range 15-15\n");
     
     for (int i = 0; i < N/2; i++)
          insert (i*2+1, &tree);

     if (member_range (14, 14, tree))
          printf ("tree contains elements in range 14-14\n");
     else
          printf ("tree does not contain elements in range 14-14\n");
     if (member_range (15, 18, tree))
          printf ("tree contains elements in range 15-18\n");
     else
          printf ("tree does not contain elements in range 15-18\n");
     if (member_range (15, 15, tree))
          printf ("tree contains elements in range 15-15\n");
     else
          printf ("tree does not contain elements in range 15-15\n");

     iterate_range (17, 17, tree);
     printf ("\n");

     iterate_range (17, 35, tree);
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
