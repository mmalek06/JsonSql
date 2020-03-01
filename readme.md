# JsonSql

JsonSql, like the name may suggest, is a scala library that you can use to query json documents using a familiar 
looking SQL syntax.

## General remarks

For now the library is only capable of handling SELECT statements. There are plans to also enable its users to 
do the rest of the basic operations, so INSERTs, UPDATEs and DELETEs, however that was not the main goal here 
so far.

## Syntax and what to watch out for

In order to be able to parse text documents correctly, some baseline syntax had to be established. When it comes to
Json itself, it may look as you like, no restrictions here. Though, when it comes to SQL, it needs to abide to
certain rules:

* Fields should always be surrounded by double quotes: "example.field".
* String constants should always be surrounded by single quotes: 'I\\'m a string constant!' (the escape sequence 
will be replaced with a single quote in the result).
* \##json## tag is mandatory - that's where the library inserts the parsed json.
* Functions should always be followed by an alias - otherwise the library won't know how to name the resulting 
column.
* In the examples below, you may notice that I'm using ALLCAPS notation for SQL keywords and function names. It's totally
optional, however given a string of SQL code, it nicely distinguishes different tags, so I prefer to use them.
* The library is not performing any kind of json code validation, which means, that for some broken json inputs
it will not blow up, but for some other it just may. At the same time it makes no promises that it will produce correct
result for incorrect json input that it didn't blew up when used on. It's the client's responsibility to 
pass it valid json code.
* The library is performing SQL code validation - to a degree. It mostly relates to the contents of where clauses.
Check out unit tests and the com.mmalek.jsonSql.execution.Filter object definition for more gory details. 
* For now, the library does not support any of: OFFSET/LIMIT, ORDER BY, nor GROUP BY clauses. I have plans to implement them, so stay tuned!

## Usage

Given json code that looks like this:
```json
{
  "id":1,
  "age":1,
  "name":"Ralph",
  "surname":"Garcia",
  "fullname":"Raymond Mann",
  "isEmployee":true,
  "address":{
      "street": "Street1",
      "city": "City1"
  }
}
```

You can run some simple queries, like this one:

```sql
SELECT "id", "age", "fullname" FROM ##json##
```

Given that everything went well, the result will be a Right:

```scala
val Right(result) = runQuery("...", json)

result("id") should be (Seq(Some(JNumber(1))))
result("age") should be (Seq(Some(JNumber(1))))
result("fullname") should be (Seq(Some(JString("Raymond Mann"))))
```

The lib can also operate on something more complex:

```json
{
  "items":[
      {
          "id":1,
          "address":{
              "street": "Street3",
              "city": "City2"
          }
      },
      {
          "id":2,
          "address": null
      },
      {
          "id":3,
          "age":30,
          "fullname":"Jack Gibson",
          "isEmployee":true,
          "address":{
              "street": "Street2",
              "city": "City1"
          }
      },
      {
          "id":4,
          "age":5,
          "fullname":"Jean Grant",
          "isEmployee":true,
          "address":{
              "street": "Street2",
              "city": "City2"
          }
      },
      {
          "id":5,
          "age":5,
          "fullname":"Debra McKenna",
          "isEmployee":false
      },
      {
          "id":6,
          "age":16,
          "fullname":"Annie Finch",
          "isEmployee":true,
          "address": null
      },
      {
          "id":7,
          "age":15,
          "fullname":"Karen Grimes",
          "isEmployee":true,
          "address": {
              "street": "Street3",
              "city": "City1"
          }
      },
      {
          "id":8,
          "age":19,
          "fullname":"Jose Wolf",
          "isEmployee":true,
          "address":{
              "street": "Street3",
              "city": "City1"
          }
      },
      {
          "id":9,
          "age":2,
          "fullname":"Hannah Teague",
          "isEmployee":true,
          "address":{
              "street": "Street2",
              "city": "City2"
          }
      },
      {
          "id":10,
          "age":29,
          "fullname":"Leo Locklear",
          "isEmployee":false,
          "address":{
              "street": "Street1",
              "city": "City1"
          }
      }
  ]
}
```

```sql
SELECT "items.id", AVG("items.age")
FROM ##json##
WHERE ("items.age" > 10 AND "items.age" < 20 AND "items.address.city" = 'City1') OR "items.isEmployee" = false
```

As you can see, the lib can also handle some basic functions - for now it's only the AVG function, but more 
will come soon. Please bear in mind that for now, it can only handle functions in the SELECT clause and putting
one elsewhere will cause a Left(error) result to be produced.

You can use the wildcard symbol (*) as well, for example:

```sql
SELECT "items.id" AS "id", "items.address.*" AS "address"
FROM ##json##
WHERE "items.age" = 15
```

will give you:

```scala
result("id") should be (Seq(None, None, None, None, None, None, Some(JNumber(7)), None, None, None))
result("address") should be (
  Seq(None, None, None, None, None, None,
    Some(
      JObject(
        Seq(
          JField("street", JString("Street3")),
          JField("city", JString("City1"))))),
    None, None, None))
```

## Other

It's not clear on first sight, but the library is trying to preserve the shape of the data fed to it. For example,
if you give it 10 objects, out of which only 5 will satisfy the condition list given in the WHERE clause, 
it will return 5 None's and 5 Some's filled with values selected - that's by design, but I've added a task to the issues list, to also create a functionality that would return only Some's.

For more use cases, look into the small test suite I wrote. 

Feel free to put any more issues you find on this github!
