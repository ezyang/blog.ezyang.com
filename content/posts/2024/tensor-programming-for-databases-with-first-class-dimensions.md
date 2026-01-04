---
title: "Tensor programming for databases, with first class dimensions"
date: 2024-10-14 01:07:14
slug: tensor-programming-for-databases-with-first-class-dimensions
categories: [PyTorch]
comments:
    - id: 33766
      author: Tom Nicholas
      date: "2024-10-15 20:42:21"
      content: |
        Every time you find yourself doing something "analogous to Pandas" but in higher dimensions, you should almost certainly just be using Xarray. I would love to see this level of effort going into better integration between Xarray and Torch!
        
        https://xarray.dev/
    - id: 33778
      author: Oliver Batchelor
      date: "2024-10-17 19:30:00"
      content: |
        Certainly curious. If you have a bunch of tensors with named dimensions, there is a lot of possible useful scope for Tensor APIs which would otherwise require in current "language" a lot of obfuscated indexing. Almost a bit like a massive generalization of einops.
        
        I've used home-built versions of TensorDict for years, and it seems useful to integrate it more closely with the core language.
    - id: 33897
      author: Ji
      date: "2024-10-30 23:52:45"
      content: |
        chatgpt told me here are some ways SQL internals optimize its performance. I think
        
        Storage matters - indexing (1) and and partitioning (3)
        Processing matters - batch /bulk (6) and parallelism (10)
        Execution matters - query based (2), caching (4), joins (8), repeated codes (9)
        I/O controls - read compressed data (5) and data correctness through concurrency /locking (7)
        
        The categories are not MECE. But it covers main areas I could personally think of. 
        
        ----
        
        1. Indexing
        
            Indexes speed up data retrieval by providing a data structure that allows the database to locate specific rows without scanning the entire table.
            B-Tree indexes and hash indexes are common, with each having benefits for certain query types.
            Clustered and non-clustered indexes can be used to prioritize frequently queried columns.
        
        2. Query Optimization
        
            Query Planner: The SQL engine uses a query planner or optimizer to determine the most efficient way to execute a query.
            Execution Plans: The optimizer generates and assesses various execution plans and chooses the most efficient one.
            Cost-based Optimization: Some systems estimate query costs based on I/O, CPU, and memory to choose the most cost-effective plan.
        
        3. Partitioning
        
            Horizontal Partitioning (Sharding): Large tables are split into smaller, manageable pieces to improve access and storage performance.
            Vertical Partitioning: Only relevant columns are retrieved, reducing memory and processing requirements.
            Range, Hash, and List Partitioning: Each type allows for optimized data retrieval based on specific patterns of access.
        
        4. Caching
        
            Buffer Pool Cache: Frequently accessed data is kept in memory, reducing disk I/O.
            Result Cache: Stores the results of frequent queries, reducing the need to recompute the results for identical queries.
            Prepared Statement Cache: Caches compiled query plans, reducing the overhead of preparing and parsing SQL statements repeatedly.
        
        5. Data Compression
        
            Reduces the storage footprint and increases I/O speed by compressing data on disk and in memory.
            Compression techniques vary but can significantly reduce the time it takes to read data from storage.
        
        6. Batch Processing and Bulk Operations
        
            Batch Insert/Update: Performing multiple rows’ operations in bulk can reduce the number of I/O operations.
            Bulk Loading: Optimized to handle large volumes of data, reducing overhead and improving load speeds.
        
        7. Concurrency Control and Locking Mechanisms
        
            Row-level locking allows multiple users to access the database simultaneously with minimal interference.
            Optimistic and Pessimistic Locking are employed based on the workload to manage conflicts and avoid bottlenecks.
        
        8. Join Algorithms and Optimizations
        
            Nested Loops, Hash Joins, and Merge Joins: Different join algorithms are selected based on table sizes and indexes, optimizing join operations.
            Early Filtering: Applying filters before joining tables helps reduce the volume of data that needs to be joined, making the process faster.
        
        9. Materialized Views
        
            Precomputed and stored query results for complex, frequently accessed queries, reducing execution time for repeated queries.
        
        10. Data Distribution and Parallel Processing
        
            Some databases split workload across multiple processors or nodes, performing tasks in parallel to accelerate large queries.
        
        Each optimization method has different performance trade-offs and is chosen based on factors like data size, query complexity, and access patterns. These techniques together make SQL databases more scalable, responsive, and efficient in handling complex queries on large datasets.
    - id: 34034
      author: Marc Laugharn
      date: "2024-11-22 23:13:47"
      content: |
        I think incremental view processing in DBs, stream calculus, coinduction, autodiff.. they all quite strongly resemble one another actually, imo. 
        
        Let Q be a query, DB be your database. Maybe you just computed Q(DB) and the database updates with change ∆DB. Just stumbled on a concept recently that makes a lot of things clear.. and feels a lot like pytorch: http://muratbuffalo.blogspot.com/2024/11/dbsp-automatic-incremental-view.html?utm_source=pocket_shared
        
        - You want to figure out Q(DB+∆DB). Maybe Q is expensive to run. How do you just update your view incrementally, without simply re-running Q all over again over the whole new DB state? 
        - What's ∆Q(DB)/∆DB? 
        - What assumption do you have to have to get a chain rule? You want one, because if you get a chain rule, it lets you know how to optimize chained expressions. 
        
        All it takes is a condition they call 'linearity' of database transactions. They introduce a programming language DBSP for modeling all of this and show that they can fully represent SQL in it.
        
        With this they show it's possible to build up complicated queries as a network of simpler circuits, i.e. the exact kind of problem that pytorch solves for users who want to build up a complicated differentiable model as a network of individual simpler modules. They represent queries using circuits of relational algebra primitives in DBSP, and show that as long as any new operator can be expressed in DBSP then 
        
        Your database queries become differentiable - I think the overlap in terminology actually reveals something profound that goes beyond analogy. 
        
        - incremental view processing  backprop
        - DBSP circuits  pytorch DAGs
        - relational operations (eg join, filter, project) as circuit nodes  tensor operations as autograd graph nodes
        - data flows in queries as circuit edges  tensors/data flowing between operations in pytorch
        - DBSP
        - as long as UDFs are linear + composable they can plug into DBSP  support for custom forward and backward in pytorch
        - join reordering  layer fusion
        - optimizing chained queries  torch.compile, kernel fusion in pytorch
        - materialized views  gradient checkpointing
        - database indexes  masked operations
        
        etc.. wild stuff before even touching anything to do w/ learning
---

Tensor libraries like PyTorch and JAX have developed compact and accelerated APIs for manipulating n-dimensional arrays. N-dimensional arrays are kind of similar to tables in database, and this results in the logical question which is could you setup a Tensor-like API to do queries on databases that would be normally done with SQL? We have two challenges:

- Tensor computation is typically uniform and data-independent. But SQL relational queries are almost entirely about filtering and joining data in a data-dependent way.
- JOINs in SQL can be thought of as performing outer joins, which is not a very common operation in tensor computation.

However, we have a secret weapon: [first class dimensions](https://github.com/facebookresearch/torchdim/blob/main/torchdim.ipynb) were primarily designed to as a new frontend syntax that made it easy to express einsum, batching and tensor indexing expressions. They might be good for SQL too.

**Representing the database.** First, how do we represent a database? A simple model following columnar database is to have every column be a distinct 1D tensor, where all columns part of the same table have a consistent indexing scheme. For simplicity, we'll assume that we support rich dtypes for the tensors (e.g., so I can have a tensor of strings). So if we consider our classic customer database of `(id, name, email)`, we would represent this as:

    customers_id: int64[C]
    customers_name: str[C]
    customers_email: str[C]

Where C is the number of the entries in the customer database. Our tensor type is written as `dtype[DIM0, DIM1, ...]`, where I reuse the name that I will use for the first class dimension that represents it. Let's suppose that the index into C does *not* coincide with id (which is good, because if they did coincide, you would have a very bad time if you ever wanted to delete an entry from the database!)

This gives us an opportunity for baby's first query: let's implement this query:

    SELECT c.name, c.email FROM customers c WHERE c.id = 1000

Notice that the result of this operation is data-dependent: it may be zero or one depending on if the id is in the database. Here is a naive implementation in standard PyTorch:

    mask = customers_id == 1000
    return (customers_name[mask], customers_email[mask])

Here, we use boolean masking to perform the data-dependent filtering operation. This implementation in eager is a bit inefficient; we materialize a full boolean mask that is then fed into the subsequent operations; you would prefer for a compiler to fuse the masking and indexing together. First class dimensions don't really help with this example, but we need to introduce some new extensions to first class dimensions. First, what we can do:

    C = dims(1)
    c_id = customers_id[C]  # {C} => int64[]
    c_name = customers_name[C]  # {C} => str[]
    c_email = customers_email[C]  # {C} => str[]
    c_mask = c_id == 1000  # {C} => bool[]

Here, a tensor with first class tensors has a more complicated type `{DIM0, DIM1, ...} => dtype[DIM2, DIM3, ...]`. The first class dimensions are all reported in the curly braces to the left of the double arrow; curly braces are used to emphasize the fact that first class dimensions are unordered.

What next? The problem is that now we want to do something like `torch.where(c_mask, c_name, ???)` but we are now in a bit of trouble, because we don't want anything in the false branch of where: we want to provide something like "null" and collapse the tensor to a smaller number of elements, much like how boolean masking did it without first class dimensions. To express this, we'll introduce a binary version of torch.where that does exactly this, as well as returning the newly allocated FCD for the new, data-dependent dimension:

    C2, c2_name = torch.where(c_mask, c_name)  # {C2} => str[]
    _C2, c2_email = torch.where(c_mask, c_email)  # {C2} => str[], n.b. C2 == _C2
    return c2_name, c2_email

Notice that torch.where introduces a new first-class dimension. I've chosen that this FCD gets memoized with `c_mask`, so whenever we do more `torch.where` invocations we still get consistently the same new FCD.

Having to type out all the columns can be a bit tiresome. If we assume all elements in a table have the same dtype (let's call it `dyn`, short for dynamic type), we can more compactly represent the table as a 2D tensor, where the first dimension is the indexing as before, and the second dimension is the columns of the database. For clarity, we'll support using the string name of the column as a shorthand for the numeric index of the column. If the tensor is contiguous, this gives a more traditional row-wise database. The new database can be conveniently manipulated with FCDs, as we can handle all of the columns at once instead of typing them out individually):

    customers:  dyn[C, C_ATTR]
    C = dims(1)
    c = customers[C]  # {C} => dyn[C_ATTR]
    C2, c2 = torch.where(c["id"] == 1000, c)  # {C2} => dyn[C_ATTR]
    return c2[["name", "email"]].order(C2)  # dyn[C2, ["name", "email"]]

We'll use this for the rest of the post, but the examples should be interconvertible.

**Aggregation.** What's the average age of all customers, grouped by the country they live in?

    SELECT AVG(c.age) FROM customers c GROUP BY c.country;

PyTorch doesn't natively support this grouping operation, but essentially what is desired here is a conversion into a **nested tensor**, where the jagged dimension is the country (each of which will have a varying number of countries). Let's hallucinate a `torch.groupby` analogous to its Pandas equivalent:

    customers: dyn[C, C_ATTR]
    customers_by_country = torch.groupby(customers, "country")  # dyn[COUNTRY, JC, C_ATTR]
    COUNTRY, JC = dims(2)
    c = customers_by_country[COUNTRY, JC]  # {COUNTRY, JC} => dyn[C_ATTR]
    return c["age"].mean(JC).order(COUNTRY)  # f32[COUNTRY]

Here, I gave the generic indexing dimension the name `JC`, to emphasize that it is a *jagged* dimension. But everything proceeds like we expect: after we've grouped the tensor and rebound its first class dimensions, we can take the field of interest and explicitly specify a reduction on the dimension we care about.

In SQL, aggregations have to operate over the entirety of groups specified by GROUP BY. However, because FCDs explicitly specify what dimensions we are reducing over, we can potentially decompose a reduction into a series of successive reductions on different columns, without having to specify subqueries to progressively perform the reductions we are interested in.

**Joins.** Given an order table, join it with the customer referenced by the customer id:

    SELECT o.id, c.name, c.email FROM orders o JOIN customers c ON o.customer_id = c.id

First class dimensions are great at doing outer products (although, like with filtering, it will expensively materialize the entire outer product naively!)

    customers: dyn[C, C_ATTR]
    orders: dyn[O, O_ATTR]
    C, O = dims(2)
    c = customers[C]  # {C} => dyn[C_ATTR]
    o = orders[O]  # {O} => dyn[O_ATTR]
    mask = o["customer_id"] == c["id"]  # {C, O} => bool[]
    outer_product = torch.cat(o[["id"]], c[["name", "email"]])  # {C, O} => dyn[["id", "name", "email"]]
    CO, co = torch.where(mask, outer_product)  # {CO} => dyn[["id", "name", "email"]]
    return co.order(CO)  # dyn[C0, ["id", "name", "email"]]

**What's the point.** There are a few reasons why we might be interested in the correspondence here. First, we might be interested in applying SQL ideas to the Tensor world: a lot of things people want to do in preprocessing are similar to what you do in traditional relational databases, and SQL can teach us what optimizations and what use cases we should think about. Second, we might be interested in applying Tensor ideas to the SQL world: in particular, I think first class dimensions are a really intuitive frontend for SQL which can be implemented entirely embedded in Python without necessitating the creation of a dedicated DSL. Also, this might be the push needed to get [TensorDict](https://github.com/pytorch/tensordict) into core.
