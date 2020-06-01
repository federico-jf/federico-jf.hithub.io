UDACITY: Final assignment
Federico Ferrero
/*Let's start with creating a table that provides the following details: actor's first and last name combined as full_name,
film title, film description and length of the movie.

How many rows are there in the table?. RTA: 5462*/
SELECT CONCAT(a.first_name, ' ', a.last_name) AS full_name, f.title, f.description, f.length
FROM film f
JOIN film_actor t
ON f.film_id = t.film_id
JOIN actor a
ON t.actor_id = a.actor_id
;

/*Write a query that creates a list of actors and movies where the movie length
was more than 60 minutes. How many rows are there in this query result?* RTA: 4900*/

SELECT CONCAT(a.first_name, ' ', a.last_name) AS full_name, f.title movie, f.length
FROM film f
JOIN film_actor t
ON f.film_id = t.film_id
JOIN actor a
ON t.actor_id = a.actor_id
WHERE f.length > 60
;
/*Write a query that captures the actor id, full name of the actor, and counts the number
of movies each actor has made. (HINT: Think about whether you should group by actor id or the full name of the actor.)
Identify the actor who has made the maximum number movies. RTA: gina degeneris*/

SELECT t.actor_id, CONCAT(a.first_name, ' ', a.last_name) AS full_name, COUNT(t.film_id) AS number_movies
FROM film f
JOIN film_actor t
ON f.film_id = t.film_id
JOIN actor a
ON t.actor_id = a.actor_id
GROUP BY 1, 2
ORDER BY 3 DESC
LIMIT 1
;

/*otra forma de resolverlo, con una subquery que optimiza el trabajo, lo hace más rapido:*/
SELECT actorid, full_name,
       COUNT(filmtitle) film_count_peractor
FROM
    (SELECT a.actor_id actorid,
            a.first_name,
            a.last_name,
            a.first_name || ' ' || a.last_name AS full_name,
            f.title filmtitle
    FROM    film_actor fa
    JOIN    actor a
    ON      fa.actor_id = a.actor_id
    JOIN    film f
    ON      f.film_id = fa.film_id) t1
GROUP BY 1, 2
ORDER BY 3 DESC

/*Write a query that displays a table with 4 columns: actor's full name, film title, length of movie, and a column name "filmlen_groups"
 that classifies movies based on their length. Filmlen_groups should include 4 categories: 1 hour or less, Between 1-2 hours, Between 2-3 hours,
 More than 3 hours.*/

 SELECT CONCAT(a.first_name, ' ', a.last_name) AS full_name, f.title movie,
 CASE WHEN f.length <= 60 THEN '1 hour or less'
      WHEN f.length > 60 AND f.length <= 120 THEN 'Between 1-2 hours'
      WHEN f.length > 120 AND f.length <= 180 THEN 'Between 2-3 hours'
      ELSE 'More than 3 hours' END AS filmlen_groups
 FROM film f
 JOIN film_actor t
 ON f.film_id = t.film_id
 JOIN actor a
 ON t.actor_id = a.actor_id;

 /*Now, we bring in the advanced SQL query concepts! Revise the query you wrote above
 to create a count of movies in each of the 4 filmlen_groups: 1 hour or less, Between 1-2 hours, Between 2-3 hours, More than 3 hours.
 OJO CON REUSAR VIEJO CODIGO, A VECES MEJOR ESCRIBIRLO DE CERO PARA RESPONDER BIEN A LA PREGUNTA*/

SELECT DISTINCT(filmlen_groups),
COUNT(title) OVER (PARTITION BY filmlen_groups) AS filmcount_bylencat
FROM
    (SELECT title,
      CASE WHEN length <= 60 THEN '1 hour or less'
      WHEN length > 60 AND length <= 120 THEN 'Between 1-2 hours'
      WHEN length > 120 AND length <= 180 THEN 'Between 2-3 hours'
      ELSE 'More than 3 hours' END AS filmlen_groups
      FROM film) sub
 ORDER BY filmlen_groups;


/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++trabajo final
SET 1 ejercicio 1*+++++++++++++++++++++++++++++++++++++++++++++++++/
/*We want to understand more about the movies that families are watching. The following categories are considered
family movies: Animation, Children, Classics, Comedy, Family and Music.

Create a query that lists each movie, the film category it is classified in, and the number of times it has been rented out.

Check Your Solution
For this query, you will need 5 tables: Category, Film_Category, Inventory, Rental and Film. Your solution should
 have three columns: Film title, Category name and Count of Rentals.

The following table header provides a preview of what the resulting table should look like if you order by
category name followed by the film title.

HINT: One way to solve this is to create a count of movies using aggregations, subqueries and Window functions.*/

SELECT title, category, count_of_rentals
FROM
    (SELECT f.title title, c.name category, r.rental_id rental_id, COUNT(r.rental_id) OVER (PARTITION BY f.title) AS count_of_rentals
    FROM category c
    JOIN film_category fc
    ON c.category_id = fc.category_id
    JOIN film f
    ON fc.film_id = f.film_id
    JOIN inventory i
    ON i.film_id = f.film_id
    JOIN rental r
    ON r.inventory_id = i.inventory_id
    WHERE c.name IN ('Animation', 'Children', 'Classics', 'Comedy', 'Family', 'Music')) sub
GROUP BY 1, 2, 3
ORDER BY 2;

/*2++++++++++++++++++++++++++++++++++
SET 1 Question 2
Now we need to know how the length of rental duration of these family-friendly movies compares to the duration that all
movies are rented for. Can you provide a table with the movie titles and divide them into 4 levels (first_quarter, second_quarter,
third_quarter, and final_quarter) based on the quartiles (25%, 50%, 75%) of the rental duration for movies across all categories?
Make sure to also indicate the category that these family-friendly movies fall into.

Check Your Solution
The data are not very spread out to create a very fun looking solution, but you should see something like the following if
you correctly split your data. You should only need the category, film_category, and film tables to answer this and the next questions.

HINT: One way to solve it requires the use of percentiles, Window functions, subqueries or temporary tables.*/


SELECT title, category_name, rental_duration, standard_quartile
FROM
    (SELECT f.title title, c.name category_name, f.rental_duration rental_duration, NTILE(4) OVER (ORDER BY f.rental_duration) AS standard_quartile
    FROM category c
    JOIN film_category fc
    ON c.category_id = fc.category_id
    JOIN film f
    ON fc.film_id = f.film_id
    WHERE c.name IN ('Animation', 'Children', 'Classics', 'Comedy', 'Family', 'Music')
    ORDER BY standard_quartile) sub;

/********************************************************3+++++++++++++++++++++++++++++++++++++++++*/
/*SET 1 Question 3
Finally, provide a table with the family-friendly film category, each of the quartiles,
and the corresponding count of movies within each combination of film category for each corresponding
 rental duration category. The resulting table should have three columns:

Category
Rental duration
Count
Check Your Solution
The following table header provides a preview of what your table should look like.
The Count column should be sorted first by Category and then by Rental Duration category.

HINT: One way to solve this question requires the use of Percentiles, Window functions and Case statements*/


SELECT category_name, standard_quartile, COUNT(category_quartile) AS count
FROM
        (SELECT category_name, standard_quartile,
        CASE WHEN category_name = 'Music' AND standard_quartile = '1' THEN 'M1'
        WHEN category_name = 'Music' AND standard_quartile = '2' THEN 'M2'
        WHEN category_name = 'Music' AND standard_quartile = '3' THEN 'M3'
        WHEN category_name = 'Music' AND standard_quartile = '4' THEN 'M4'
        WHEN category_name = 'Animation' AND standard_quartile = '1' THEN 'A1'
        WHEN category_name = 'Animation' AND standard_quartile = '2' THEN 'A2'
        WHEN category_name = 'Animation' AND standard_quartile = '3' THEN 'A3'
        WHEN category_name = 'Animation' AND standard_quartile = '4' THEN 'A4'
        WHEN category_name = 'Children' AND standard_quartile = '1' THEN 'C1'
        WHEN category_name = 'Children' AND standard_quartile = '2' THEN 'C2'
        WHEN category_name = 'Children' AND standard_quartile = '3' THEN 'C3'
        WHEN category_name = 'Children' AND standard_quartile = '4' THEN 'C4'
        WHEN category_name = 'Classics' AND standard_quartile = '1' THEN 'CL1'
        WHEN category_name = 'Classics' AND standard_quartile = '2' THEN 'CL2'
        WHEN category_name = 'Classics' AND standard_quartile = '3' THEN 'CL3'
        WHEN category_name = 'Classics' AND standard_quartile = '4' THEN 'CL4'
        WHEN category_name = 'Comedy' AND standard_quartile = '1' THEN 'CO1'
        WHEN category_name = 'Comedy' AND standard_quartile = '2' THEN 'CO2'
        WHEN category_name = 'Comedy' AND standard_quartile = '3' THEN 'CO3'
        WHEN category_name = 'Comedy' AND standard_quartile = '4' THEN 'CO4'
        WHEN category_name = 'Family' AND standard_quartile = '1' THEN 'F1'
        WHEN category_name = 'Family' AND standard_quartile = '2' THEN 'F2'
        WHEN category_name = 'Family' AND standard_quartile = '3' THEN 'F3'
        WHEN category_name = 'Family' AND standard_quartile = '4' THEN 'F4'  END AS category_quartile
        FROM (SELECT f.title title, c.name category_name, f.rental_duration rental_duration, NTILE(4) OVER (ORDER BY f.rental_duration) AS standard_quartile
              FROM category c
              JOIN film_category fc
              ON c.category_id = fc.category_id
              JOIN film f
              ON fc.film_id = f.film_id
              WHERE c.name IN ('Animation', 'Children', 'Classics', 'Comedy', 'Family', 'Music')
              ORDER BY standard_quartile) sub) sub2
GROUP BY 1, 2
ORDER BY 1, 2;


/*+++++++++++++++++++++++++++++++++++++++++++++++/

/* SET 2 Question 1:
We want to find out how the two stores compare in their count of rental orders during every month for all the years we have data for.
Write a query that returns the store ID for the store, the year and month and the number of rental orders each store has fulfilled for
 that month. Your table should include a column for each of the following: year, month, store ID and count of rental orders fulfilled during that month.

Check Your Solution
The following table header provides a preview of what your table should look like. The count of rental orders is sorted in descending order.*/


SELECT DATE_PART('month', r.rental_date) AS rental_month,
       DATE_PART('year', r.rental_date) AS rental_year,
       s.store_id store_id,
       COUNT(r.rental_id) AS count_rentals
FROM staff s
JOIN rental r
ON r.staff_id = s.staff_id
GROUP BY 1, 2, 3
ORDER BY 4 DESC;


/*SET 2 Question 2
We would like to know who were our top 10 paying customers, how many payments they made on a monthly basis during 2007,
and what was the amount of the monthly payments. Can you write a query to capture the customer name,
month and year of payment, and total payment amount for each month by these top 10 paying customers?

Check your Solution:
The following table header provides a preview of what your table should look like. The results are sorted
first by customer name and then for each month. As you can see, total amounts per month are listed for each customer.

HINT: One way to solve is to use a subquery, limit within the subquery, and use concatenation to generate the customer name.*/

WITH  tab1 AS (SELECT customer_id
      FROM payment
      GROUP BY 1
      ORDER BY SUM(amount) DESC
      LIMIT 10),


      tab2 AS
      (SELECT DATE_TRUNC('month', payment_date) AS pay_mon,
      CONCAT (first_name,' ', last_name) AS fullname,
      COUNT(*) AS pay_countpermon,
      SUM(amount) AS pay_amount
      FROM customer c
      JOIN tab1
      ON c.customer_id = tab1.customer_id
      JOIN payment p
      ON tab1.customer_id = p.customer_id
      GROUP BY 1, 2
      ORDER BY 2, 1)

SELECT pay_mon,
     fullname,
     pay_countpermon,
     pay_amount
FROM tab2
LIMIT 10;


/*++++++++++++++++++++++++++++++++++++++
SET 2 Question 3
Finally, for each of these top 10 paying customers, I would like to find out the difference across
their monthly payments during 2007. Please go ahead and write a query to compare the payment amounts in each
successive month. Repeat this for each of these 10 paying customers. Also, it will be tremendously helpful if
you can identify the customer name who paid the most difference in terms of payments.

Check your solution:
The customer Eleanor Hunt paid the maximum difference of $64.87 during March 2007 from $22.95 in February of 2007.

HINT: You can build on the previous questions query to add Window functions and aggregations to get the solution.
*/
WITH  tab1 AS (SELECT customer_id
      FROM payment
      GROUP BY 1
      ORDER BY SUM(amount) DESC
      LIMIT 10),


      tab2 AS
      (SELECT DATE_TRUNC('month', payment_date) AS pay_mon,
      CONCAT (first_name,' ', last_name) AS fullname,
      COUNT(*) AS pay_countpermon,
      SUM(amount) AS pay_amount
      FROM customer c
      JOIN tab1
      ON c.customer_id = tab1.customer_id
      JOIN payment p
      ON tab1.customer_id = p.customer_id
      GROUP BY 1, 2
      ORDER BY 2, 1)

SELECT pay_mon,
     fullname,
     pay_amount,
     pay_amount - LAG(pay_amount) OVER (PARTITION BY fullname ORDER BY pay_mon) AS lag_difference
FROM tab2
ORDER BY 4 DESC;
