package com.logicaldoc.core;

import java.sql.ResultSet;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.jdbc.core.RowMapper;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;

import jakarta.transaction.Transactional;

/**
 * Interface for DAOs that operate on persistent objects
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 *
 * @param <T> Class of the implementation of a {@link PersistentObject} this DAO
 *        handles
 */
@Transactional
public interface PersistentObjectDAO<T extends PersistentObject> {

	/**
	 * The alias to use to reference the object in the queries
	 */
	public static final String ENTITY = "_entity";

	/**
	 * This method persists the entity object
	 * 
	 * @param entity entity to be stored
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public void store(T entity) throws PersistenceException;

	/**
	 * This method finds an entity by ID
	 * 
	 * @param id ID of the entity
	 * 
	 * @return Entity with given ID
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public T findById(long id) throws PersistenceException;

	/**
	 * This method finds an entity by ID
	 * 
	 * @param id ID of the entity
	 * @param initialize True if the instance's lazy collections have to be
	 *        initialized
	 * 
	 * @return Entity with given ID
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public default T findById(long id, boolean initialize) throws PersistenceException {
		T entity = findById(id);
		if (initialize)
			initialize(entity);
		return entity;
	}

	/**
	 * Finds all entities in the database
	 * 
	 * @return The list of all entities
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public default List<T> findAll() throws PersistenceException {
		return findByWhere("", "", null);
	}

	/**
	 * Finds all entities in a specific tenant.
	 * 
	 * @param tenantId Identifier of the tenant to search in
	 * 
	 * @return The list of all entities
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public default List<T> findAll(long tenantId) throws PersistenceException {
		return findByWhere(" " + ENTITY + ".tenantId=" + tenantId, "", null);
	}

	/**
	 * Finds all entities ids
	 * 
	 * @return The list of all entities ids
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public default List<Long> findAllIds() throws PersistenceException {
		return findIdsByWhere("", "", null);
	}

	/**
	 * Finds all entities ids in a specific tenant.
	 * 
	 * @param tenantId Identifier of the tenant to search in
	 * 
	 * @return The list of all entities ids
	 * 
	 * @throws PersistenceException Error in the database
	 */
	public default List<Long> findAllIds(long tenantId) throws PersistenceException {
		return findIdsByWhere(" " + ENTITY + ".tenantId=" + tenantId, "", null);
	}

	/**
	 * Finds all entities by the given expression. Use {@value #ENTITY} alias to
	 * reference attributes in the where expression.
	 * 
	 * @param where The where clause expression
	 * @param order The order clause expression
	 * @param max Maximum results number (optional)
	 * 
	 * @return The list of marching entities
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public default List<T> findByWhere(String where, String order, Integer max) throws PersistenceException {
		return findByWhere(where, (Map<String, Object>) null, order, max);
	}

	/**
	 * Finds all entities by the given expression. Use {@value #ENTITY} alias to
	 * reference attributes in the where expression.
	 * 
	 * @param where The where clause expression
	 * @param parameters Parameters used in the where expression
	 * @param order The order clause expression
	 * @param max Maximum results number (optional)
	 * 
	 * @return The list of marching entities
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public List<T> findByWhere(String where, Map<String, Object> parameters, String order, Integer max)
			throws PersistenceException;

	/**
	 * Finds all entities by the given object query.
	 * 
	 * @param query The query expression
	 * @param parameters Parameters used in the where expression
	 * @param max Maximum results number (optional)
	 * 
	 * @return The list of matching entities
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public List<T> findByObjectQuery(String query, Map<String, Object> parameters, Integer max)
			throws PersistenceException;

	/**
	 * Find everything you want from the DB using the ORM query language
	 * 
	 * @param query The query to execute
	 * @param parameters The map of the parameters
	 * @param max Maximum results number (optional)
	 * 
	 * @return Query result
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public List<Object[]> findByQuery(String query, Map<String, Object> parameters, Integer max)
			throws PersistenceException;

	/**
	 * Find everything you want from the DB using the ORM query language
	 * 
	 * @param query The query to execute
	 * @param parameters The map of the parameters
	 * @param requiredType The type of the elements in the result
	 * @param max Maximum results number (optional)
	 * 
	 * @return Query result
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public <R> List<R> findByQuery(String query, Map<String, Object> parameters, Class<R> requiredType, Integer max)
			throws PersistenceException;

	/**
	 * Finds all entities ids by the given expression. Use {@value #ENTITY}
	 * alias to reference attributes in the where expression.
	 * 
	 * @param where The where clause expression
	 * @param order The order clause expression
	 * @param max Maximum results number (optional)
	 * 
	 * @return The list of marching entities ids
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public default List<Long> findIdsByWhere(String where, String order, Integer max) throws PersistenceException {
		return findIdsByWhere(where, new HashMap<>(), order, max);
	}

	/**
	 * Finds all entities ids by the given expression. Use {@value #ENTITY}
	 * alias to reference attributes in the where expression.
	 * 
	 * @param where The where clause expression (for parameters, please use
	 *        JPA-style: :paramA, :paramB ...)
	 * @param parameters The map of the parameters
	 * @param order The order clause expression
	 * @param max Maximum results number (optional)
	 * 
	 * @return The list of marching entities ids
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public List<Long> findIdsByWhere(String where, Map<String, Object> parameters, String order, Integer max)
			throws PersistenceException;

	/**
	 * Initialises lazy loaded data such as collections
	 * 
	 * @param entity The entity to be initialised
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public void initialize(T entity) throws PersistenceException;

	/**
	 * Initialises lazy loaded data such as collections of all specified
	 * entities
	 * 
	 * @param entities The entities to be initialize
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public default void initialize(Collection<T> entities) throws PersistenceException {
		for (T entity : entities)
			initialize(entity);
	}
	
	/**
	 * Counts the total number of not-deleted entities
	 * 
	 * @return The total count
	 * 
	 * @throws PersistenceException Error in the database 
	 */
	public default long count()  throws PersistenceException {
		return count(null);
	}

	/**
	 * Counts the total number of not-deleted entities
	 * 
	 * @param tenantId Optional specification of the tenant
	 * 
	 * @return The total count
	 * 
	 * @throws PersistenceException Error in the database 
	 */
	public default long count(Long tenantId)  throws PersistenceException {
		return count(tenantId, false);
	}
	
	/**
	 * Counts the total number of entities
	 * 
	 * @param tenantId Optional specification of the tenant
	 * @param computeDeleted If deleted records must be computed as well
	 * 
	 * @return The total count
	 * 
	 * @throws PersistenceException Error in the database 
	 */
	public long count(Long tenantId, boolean computeDeleted)  throws PersistenceException;
	
	/**
	 * Query given SQL to create a prepared statement from SQL and a list of
	 * arguments to bind to the query, mapping each row to a Java object via a
	 * RowMapper.
	 * 
	 * @param sql SQL query to execute
	 * @param maxRows the new max rows limit; null means there is no limit
	 * @param rowMapper object that will map one object per row
	 * 
	 * @return the result List, containing mapped objects
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public default <P> List<P> query(String sql, RowMapper<P> rowMapper, Integer maxRows) throws PersistenceException {
		return query(sql, null, rowMapper, maxRows);
	}

	/**
	 * Query given SQL to create a prepared statement from SQL and a list of
	 * arguments to bind to the query, mapping each row to a Java object via a
	 * RowMapper.
	 * 
	 * @param sql SQL query to execute (for parameters please use JPA-style:
	 *        :paramA, :paramB ...)
	 * @param parameters Parameters used in the where expression (map
	 *        name-value)
	 * @param maxRows the new max rows limit; null means there is no limit
	 * @param rowMapper object that will map one object per row
	 * 
	 * @return the result List, containing mapped objects
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public <P> List<P> query(String sql, Map<String, Object> parameters, RowMapper<P> rowMapper, Integer maxRows)
			throws PersistenceException;

	/**
	 * Query given SQL to create a prepared statement from SQL and a list of
	 * arguments to bind to the query, you can give your own worker to iterate
	 * the {@link ResultSet}.
	 * 
	 * @param sql SQL query to execute (for parameters please use JPA-style:
	 *        :paramA, :paramB ...)
	 * @param parameters Parameters used in the where expression (map
	 *        name-value)
	 * @param maxRows the new max rows limit; null means there is no limit
	 * @param worker an implementation that receive the {@link ResultSet} to
	 *        iterate
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public void queryForResultSet(String sql, Map<String, Object> parameters, Integer maxRows, ResultSetWalker worker)
			throws PersistenceException;

	/**
	 * Query given SQL to create a prepared statement from SQL and a list of
	 * arguments to bind to the query, expecting a result list. The results will
	 * be mapped to a List (one entry for each row) of result objects, each of
	 * them matching the specified element type.
	 * 
	 * @param sql SQL query to execute (for parameters please use JPA-style:
	 *        :paramA, :paramB ...)
	 * @param parameters Parameters used in the where expression (map
	 *        name-value)
	 * @param requiredType the required type of element in the result list (for
	 *        example, Integer.class)
	 * @param maxRows maximum number of returned records
	 * 
	 * @return a List of objects that match the specified element type
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public <R> List<R> queryForList(String sql, Map<String, Object> parameters, Class<R> requiredType, Integer maxRows)
			throws PersistenceException;

	/**
	 * Execute a query for a result list, given static SQL. Uses a JDBC
	 * Statement, not a PreparedStatement. If you want to execute a static query
	 * with a PreparedStatement, use the overloaded queryForList method with
	 * null as argument array. The results will be mapped to a List (one entry
	 * for each row) of result objects, each of them matching the specified
	 * element type.
	 * 
	 * @param sql SQL query to execute
	 * @param requiredType the required type of element in the result list (for
	 *        example, Integer.class)
	 * @param maxRows maximum number of returned records
	 * 
	 * @return a List of objects that match the specified element type
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public <R> List<R> queryForList(String sql, Class<R> requiredType, Integer maxRows) throws PersistenceException;

	/**
	 * Query given SQL to create a prepared statement from SQL and a list of
	 * arguments to bind to the query, expecting a result list. The results will
	 * be mapped to a List (one entry for each row) of result objects, each of
	 * them matching the specified element type.
	 * 
	 * @param sql SQL query to execute
	 * @param requiredType the required type of element in the result list (for
	 *        example, Integer.class)
	 * 
	 * @return a List of objects that match the specified element type
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public <R> List<R> queryForList(String sql, Class<R> requiredType) throws PersistenceException;

	/**
	 * Execute a query that results in an int value, given static SQL. Uses a
	 * JDBC Statement, not a PreparedStatement. If you want to execute a static
	 * query with a PreparedStatement, use the overloaded queryForInt method
	 * with null as argument array. This method is useful for running static SQL
	 * with a known outcome. The query is expected to be a single row/single
	 * column query that results in an int value.
	 * 
	 * @param sql SQL query to execute
	 * 
	 * @return the int value, or 0 in case of SQL NULL
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public int queryForInt(String sql) throws PersistenceException;

	/**
	 * Execute a query that results in an int value, given static SQL. Uses a
	 * JDBC Statement, not a PreparedStatement. If you want to execute a static
	 * query with a PreparedStatement, use the overloaded queryForInt method
	 * with null as argument array. This method is useful for running static SQL
	 * with a known outcome. The query is expected to be a single row/single
	 * column query that results in an int value.
	 * 
	 * @param sql SQL query to execute
	 * @param parameters Parameters used in the where expression (map
	 *        name-value)
	 * 
	 * @return the int value, or 0 in case of SQL NULL
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public int queryForInt(String sql, Map<String, Object> parameters) throws PersistenceException;

	/**
	 * Execute a query that results in an long value, given static SQL. Uses a
	 * JDBC Statement, not a PreparedStatement. If you want to execute a static
	 * query with a PreparedStatement, use the overloaded queryForInt method
	 * with null as argument array. This method is useful for running static SQL
	 * with a known outcome. The query is expected to be a single row/single
	 * column query that results in a long value.
	 * 
	 * @param sql SQL query to execute
	 * 
	 * @return the long value, or 0 in case of SQL NULL
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public long queryForLong(String sql) throws PersistenceException;

	/**
	 * Execute a query that results in an long value, given static SQL. Uses a
	 * JDBC Statement, not a PreparedStatement. If you want to execute a static
	 * query with a PreparedStatement, use the overloaded queryForInt method
	 * with null as argument array. This method is useful for running static SQL
	 * with a known outcome. The query is expected to be a single row/single
	 * column query that results in a long value.
	 * 
	 * @param sql SQL query to execute (for parameters please use JPA-style:
	 *        :paramA, :paramB ...)
	 * @param parameters Parameters used in the where expression (map
	 *        name-value)
	 * 
	 * @return the long value, or 0 in case of SQL NULL
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public long queryForLong(String sql, Map<String, Object> parameters) throws PersistenceException;

	/**
	 * Execute a query that results in a double value, given static SQL. Uses a
	 * JDBC Statement, not a PreparedStatement. If you want to execute a static
	 * query with a PreparedStatement, use the overloaded queryForInt method
	 * with null as argument array. This method is useful for running static SQL
	 * with a known outcome. The query is expected to be a single row/single
	 * column query that results in a long value.
	 * 
	 * @param sql SQL query to execute
	 * 
	 * @return the double value, or 0 in case of SQL NULL
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public double queryForDouble(String sql) throws PersistenceException;

	/**
	 * Execute a query that results in a double value, given static SQL. Uses a
	 * JDBC Statement, not a PreparedStatement. If you want to execute a static
	 * query with a PreparedStatement, use the overloaded queryForInt method
	 * with null as argument array. This method is useful for running static SQL
	 * with a known outcome. The query is expected to be a single row/single
	 * column query that results in a long value.
	 * 
	 * @param sql SQL query to execute (for parameters please use JPA-style:
	 *        :paramA, :paramB ...)
	 * @param parameters Parameters used in the where expression (map
	 *        name-value)
	 * 
	 * @return the double value, or 0 in case of SQL NULL
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public double queryForDouble(String sql, Map<String, Object> parameters) throws PersistenceException;

	/**
	 * Execute a query that results in an string value, given static SQL. Uses a
	 * JDBC Statement, not a PreparedStatement. If you want to execute a static
	 * query with a PreparedStatement, use the overloaded queryForInt method
	 * with null as argument array. This method is useful for running static SQL
	 * with a known outcome. The query is expected to be a single row/single
	 * column query that results in a string value.
	 * 
	 * @param sql SQL query to execute
	 * 
	 * @return the string value
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public String queryForString(String sql) throws PersistenceException;

	/**
	 * Execute a query that results in an string value, given static SQL. Uses a
	 * JDBC Statement, not a PreparedStatement. If you want to execute a static
	 * query with a PreparedStatement, use the overloaded queryForInt method
	 * with null as argument array. This method is useful for running static SQL
	 * with a known outcome. The query is expected to be a single row/single
	 * column query that results in a string value.
	 * 
	 * @param sql SQL query to execute (for parameters please use JPA-style:
	 *        :paramA, :paramB ...)
	 * @param parameters Parameters used in the where expression (map
	 *        name-value)
	 * 
	 * @return the string value
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public String queryForString(String sql, Map<String, Object> parameters) throws PersistenceException;

	/**
	 * Execute a query that results in a Object value, given static SQL. Uses a
	 * JDBC Statement, not a PreparedStatement. If you want to execute a static
	 * query with a PreparedStatement. This method is useful for running static
	 * SQL with a known outcome. The query is expected to be a single row/single
	 * column query that results in a object value.
	 * 
	 * @param sql SQL query to execute
	 * @param requiredType The type of the returned value
	 * 
	 * @return the object value
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public <R> R queryForObject(String sql, Class<R> requiredType) throws PersistenceException;

	/**
	 * Execute a query that results in a Object value, given static SQL. Uses a
	 * JDBC Statement, not a PreparedStatement. If you want to execute a static
	 * query with a PreparedStatement. This method is useful for running static
	 * SQL with a known outcome. The query is expected to be a single row/single
	 * column query that results in a object value.
	 * 
	 * @param sql SQL query to execute
	 * @param parameters Optional map of parameters
	 * @param requiredType The type of the returned value
	 * 
	 * @return the object value
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public <R> R queryForObject(String sql, Map<String, Object> parameters, Class<R> requiredType)
			throws PersistenceException;

	/**
	 * This method deletes an entity. Same as delete(id,
	 * {@link PersistentObject#DELETED_CODE_DEFAULT})
	 * 
	 * @param id ID of the entity which should be deleted.
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public default void delete(long id) throws PersistenceException {
		delete(id, PersistentObject.DELETED_CODE_DEFAULT);
	}

	/**
	 * This method deletes an entity and you can give a deletion code
	 * 
	 * @param id ID of the entity which should be deleted
	 * @param code Deletion code
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public void delete(long id, int code) throws PersistenceException;

	/**
	 * Deletes all entries form the database. Same as deleteAll(entities,
	 * {@link PersistentObject#DELETED_CODE_DEFAULT})
	 * 
	 * @param entities The entities to be deleted
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public default void deleteAll(Collection<T> entities) throws PersistenceException {
		deleteAll(entities, PersistentObject.DELETED_CODE_DEFAULT);
	}

	/**
	 * Deletes all entries form the database giving a specific deletion code
	 * 
	 * @param entities The entities to be deleted
	 * @param code The deletion code
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public void deleteAll(Collection<T> entities, int code) throws PersistenceException;

	/**
	 * Removes an entity from current session and cache
	 * 
	 * @param obj Class of the entity to evict
	 * @param id Identifier of the entity
	 */
	public void evict(Class<? extends PersistentObject> obj, long id);

	/**
	 * Removes an entity from current session and cache
	 * 
	 * @param id Identifier of the entity
	 */
	public void evict(long id);

	/**
	 * Executes a bulk update as specified by the given HQL expression
	 * 
	 * @param expression The update expression.
	 * @param parameters Optional map of parameters
	 * 
	 * @return the number of modified records
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public int bulkUpdate(String expression, Map<String, Object> parameters) throws PersistenceException;

	/**
	 * Executes the given SQL update statement
	 * 
	 * @param sql the SQL statement to execute against the database
	 * 
	 * @return the value returned by the database after execution
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public int jdbcUpdate(String sql) throws PersistenceException;

	/**
	 * Issue a single SQL update operation (such as an insert, update or delete
	 * statement) via a prepared statement, binding the given arguments
	 * 
	 * @param sql SQL statement to execute (for parameters please use JPA-style:
	 *        :paramA, :paramB ...)
	 * @param parameters Parameters used in the where expression (map
	 *        name-value)
	 * 
	 * @return the number of rows affected
	 * 
	 * @throws PersistenceException raised in case of errors in the database
	 */
	public int jdbcUpdate(String sql, Map<String, Object> parameters) throws PersistenceException;

	/**
	 * Get the DBMS name currently connected(possible values are: <b>mysql</b>,
	 * <b>mariadb</b>, <b>postgresql</b>, <b>hsqldb</b>, <b>oracle</b>,
	 * <b>mssql</b>)
	 * 
	 * @return the database identifier
	 */
	public default String getDbms() {
		ContextProperties config = Context.get().getConfig();
		return config.getProperty("jdbc.dbms", "mysql").toLowerCase();
	}

	public default boolean isOracle() {
		return "oracle".equals(getDbms());
	}

	public default boolean isMySQL() {
		return "mysql".equals(getDbms()) || isMariaDB();
	}
	
	public default boolean isMariaDB() {
		return "maria".equals(getDbms());
	}

	/**
	 * Retrieves the metadata from the database
	 * 
	 * @return a map of metadata from the database driver
	 */
	public Map<String, String> getDatabaseMetadata();
}