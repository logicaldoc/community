package com.logicaldoc.core;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import javax.sql.DataSource;

import org.apache.commons.lang.StringUtils;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.query.Query;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import com.logicaldoc.core.metadata.Attribute;
import com.logicaldoc.core.metadata.ExtensibleObject;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.config.ContextProperties;

/**
 * Hibernate implementation of <code>PersistentObjectDAO</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.0
 */
public abstract class HibernatePersistentObjectDAO<T extends PersistentObject> implements PersistentObjectDAO<T> {

	private static final String UPDATE = "update ";

	private static final String DATA_SOURCE = "DataSource";

	private static final String PARAM = ":param";

	private static final String AND = " and (";

	private static final String ORDER_BY = "order by";

	protected Logger log = LoggerFactory.getLogger(HibernatePersistentObjectDAO.class);

	protected Class<T> entityClass;

	protected SessionFactory sessionFactory;

	protected static final String ASPECT_STORING = "storing";

	private static final String DEFAULT_WHERE_PREAMBLE = " " + ENTITY + " where " + ENTITY + ".deleted=0 ";

	public void setSessionFactory(SessionFactory sessionFactory) {
		this.sessionFactory = sessionFactory;
	}

	protected HibernatePersistentObjectDAO(Class<T> entityClass) {
		super();
		this.entityClass = entityClass;
	}

	public void delete(long id, int code) throws PersistenceException {
		assert (code != 0);

		if (!checkStoringAspect())
			return;

		T entity = findById(id);
		if (entity == null)
			return;
		entity.setDeleted(code);
		store(entity);
	}

	public void delete(long id) throws PersistenceException {
		delete(id, PersistentObject.DELETED_CODE_DEFAULT);
	}

	public List<T> findAll() {
		try {
			return findByWhere("", "", null);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	public List<T> findAll(long tenantId) {
		try {
			return findByWhere(" " + ENTITY + ".tenantId=" + tenantId, "", null);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	public List<Long> findAllIds() {
		try {
			return findIdsByWhere("", "", null);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	public List<Long> findAllIds(long tenantId) {
		try {
			return findIdsByWhere(" " + ENTITY + ".tenantId=" + tenantId, "", null);
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			return new ArrayList<>();
		}
	}

	@Override
	public T findById(long id, boolean initialize) throws PersistenceException {
		T entity = findById(id);
		if (initialize)
			initialize(entity);
		return entity;
	}

	@Override
	public T findById(long id) throws PersistenceException {
		T entity = null;
		try {
			entity = sessionFactory.getCurrentSession().get(entityClass, id);
			if (entity != null && entity.getDeleted() == 1)
				return null;
			return entity;
		} catch (Throwable e) {
			if (e instanceof PersistenceException)
				throw (PersistenceException) e;
			else
				throw new PersistenceException(e);
		}
	}

	@Override
	public List<T> findByWhere(String where, String order, Integer max) throws PersistenceException {
		return findByWhere(where, (Map<String, Object>) null, order, max);
	}

	@Override
	public List<T> findByWhere(String where, Object[] values, String order, Integer max) throws PersistenceException {
		List<T> coll = new ArrayList<>();
		try {
			String sorting = StringUtils.isNotEmpty(order) && !order.toLowerCase().contains(ORDER_BY)
					? ORDER_BY + " " + order
					: order;
			String query = "from " + entityClass.getCanonicalName() + DEFAULT_WHERE_PREAMBLE
					+ (StringUtils.isNotEmpty(where) ? AND + where + ") " : " ")
					+ (StringUtils.isNotEmpty(sorting) ? sorting : " ");
			coll = findByObjectQuery(query, values, max);
			return coll;
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@Override
	public List<T> findByObjectQuery(String query, Object[] values, Integer max) throws PersistenceException {
		List<T> coll = new ArrayList<>();
		try {
			logQuery(query);
			Query<T> queryObject = prepareQueryForObject(query, values, max);
			coll = queryObject.list();
			return coll;
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@Override
	public List<T> findByWhere(String where, Map<String, Object> parameters, String order, Integer max)
			throws PersistenceException {
		List<T> coll = new ArrayList<>();
		try {
			String sorting = StringUtils.isNotEmpty(order) && !order.toLowerCase().contains(ORDER_BY)
					? ORDER_BY + " " + order
					: order;
			String query = "from " + entityClass.getCanonicalName() + DEFAULT_WHERE_PREAMBLE
					+ (StringUtils.isNotEmpty(where) ? AND + where + ") " : " ")
					+ (StringUtils.isNotEmpty(sorting) ? sorting : " ");
			coll = findByObjectQuery(query, parameters, max);
			return coll;
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@Override
	public List<T> findByObjectQuery(String query, Map<String, Object> parameters, Integer max)
			throws PersistenceException {
		try {
			logQuery(query);
			Query<T> queryObject = prepareQueryForObject(query, parameters, max);
			return queryObject.list();
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	protected void logQuery(String query) {
		log.debug("Execute query: {}", query);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List findByQuery(String query, Object[] values, Integer max) throws PersistenceException {
		try {
			logQuery(query);
			Query queryObject = prepareQuery(query, values, max);
			return queryObject.list();
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public List findByQuery(String query, Map<String, Object> parameters, Integer max) throws PersistenceException {
		try {
			logQuery(query);
			Query queryObject = prepareQuery(query, parameters, max);
			return queryObject.list();
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@Override
	public List<Long> findIdsByWhere(String where, String order, Integer max) throws PersistenceException {
		return findIdsByWhere(where, new Object[0], order, max);
	}

	@Override
	public List<Long> findIdsByWhere(String where, Object[] values, String order, Integer max)
			throws PersistenceException {
		try {
			String sorting = StringUtils.isNotEmpty(order) && !order.toLowerCase().contains(ORDER_BY)
					? ORDER_BY + " " + order
					: order;
			String query = "select " + ENTITY + ".id from " + entityClass.getCanonicalName() + DEFAULT_WHERE_PREAMBLE
					+ (StringUtils.isNotEmpty(where) ? AND + where + ") " : " ")
					+ (StringUtils.isNotEmpty(sorting) ? sorting : " ");
			logQuery(query);
			Query<Long> queryObject = prepareQueryForLong(query, values, max);
			return queryObject.list();
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	/**
	 * Checks if the aspect for storing data is enabled
	 */
	protected boolean checkStoringAspect() {
		if (!RunLevel.current().aspectEnabled(ASPECT_STORING)) {
			log.error("Apect {} is disabled", ASPECT_STORING);
			return false;
		}
		return true;
	}

	public void store(T entity) throws PersistenceException {
		if (!checkStoringAspect())
			return;
		entity.setLastModified(new java.util.Date());

		// Save the entity
		try {
			saveOrUpdate(entity);
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	protected void saveOrUpdate(Object entity) {
		// Update the attributes
		if (entity instanceof ExtensibleObject) {
			try {
				ExtensibleObject extensibleEntity = (ExtensibleObject) entity;
				for (String name : extensibleEntity.getAttributes().keySet()) {
					Attribute att = extensibleEntity.getAttribute(name);
					if (att.getMultiple() == 1 && att.getType() == Attribute.TYPE_STRING) {
						String vals = extensibleEntity.getValues(name).stream().map(n -> n.toString())
								.collect(Collectors.joining(","));
						att.setStringValues(vals);
					} else
						att.setStringValues(null);
				}
			} catch (Throwable t) {
				// Nothing to do
			}
		}

		sessionFactory.getCurrentSession().saveOrUpdate(entity);
	}

	protected void flush() {
		try {
			sessionFactory.getCurrentSession().flush();
		} catch (Exception e) {
			// Nothing to do

		}
	}

	protected void refresh(Object entity) {
		if (entity == null)
			return;

		try {
			if (!sessionFactory.getCurrentSession().contains(entity)) {
				sessionFactory.getCurrentSession().refresh(entity);
			}
		} catch (Exception e) {
			// Nothing to do
		}
	}

	protected Object merge(Object entity) {
		try {
			return sessionFactory.getCurrentSession().merge(entity);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
			return null;
		}
	}

	protected void evict(Object entity) {
		sessionFactory.getCurrentSession().evict(entity);
	}

	/**
	 * Utility method useful for preparing an Hibernate query for updates
	 * 
	 * @param expression The expression for the query
	 * @param values The parameters values to be used (optional, if the query is
	 *        parametric)
	 * @param max Optional maximum number of wanted results
	 * 
	 * @return The Hibernate query
	 */
	@SuppressWarnings("rawtypes")
	protected Query prepareQueryForUpdate(String expression, Object[] values, Integer max) {
		if (values != null)
			for (int i = 0; i < values.length; i++)
				expression = expression.replace("?" + (i + 1), PARAM + (i + 1));
		Query queryObject = sessionFactory.getCurrentSession().createQuery(expression);
		applyParamsAndLimit(values, max, queryObject);
		return queryObject;
	}

	/**
	 * Utility method useful for preparing an Hibernate query for objects of
	 * this type
	 * 
	 * @param expression The expression for the query
	 * @param values The parameters values to be used (optional, if the query is
	 *        parametric)
	 * @param max Optional maximum number of wanted results
	 * 
	 * @return The Hibernate query
	 */
	protected Query<T> prepareQueryForObject(String expression, Object[] values, Integer max) {
		if (values != null)
			for (int i = 0; i < values.length; i++)
				expression = expression.replace("?" + (i + 1), PARAM + (i + 1));
		Query<T> queryObject = sessionFactory.getCurrentSession().createQuery(expression, entityClass);
		applyParamsAndLimit(values, max, queryObject);
		return queryObject;
	}

	/**
	 * Utility method useful for preparing an Hibernate query for generic result
	 * 
	 * @param expression The expression for the query
	 * @param values The parameters values to be used (optional, if the query is
	 *        parametric)
	 * @param max Optional maximum number of wanted results
	 * 
	 * @return The Hibernate query
	 */
	protected Query<Object[]> prepareQuery(String expression, Object[] values, Integer max) {
		if (values != null)
			for (int i = 0; i < values.length; i++)
				expression = expression.replace("?" + (i + 1), PARAM + (i + 1));
		Query<Object[]> queryObject = sessionFactory.getCurrentSession().createQuery(expression, Object[].class);
		applyParamsAndLimit(values, max, queryObject);
		return queryObject;
	}

	/**
	 * Utility method useful for preparing an Hibernate query for longs
	 * 
	 * @param expression The expression for the query
	 * @param values The parameters values to be used (optional, if the query is
	 *        parametric)
	 * @param max Optional maximum number of wanted results
	 * 
	 * @return The Hibernate query
	 */
	protected Query<Long> prepareQueryForLong(String expression, Object[] values, Integer max) {
		if (values != null)
			for (int i = 0; i < values.length; i++)
				expression = expression.replace("?" + (i + 1), PARAM + (i + 1));
		Query<Long> queryObject = sessionFactory.getCurrentSession().createQuery(expression, Long.class);
		applyParamsAndLimit(values, max, queryObject);
		return queryObject;
	}

	/**
	 * Applies the params
	 * 
	 * @param values the values
	 * @param max max number of results
	 * @param queryObject the query
	 * 
	 * @deprecated
	 */
	@Deprecated(since = "8.9")
	private void applyParamsAndLimit(Object[] values, Integer max, @SuppressWarnings("rawtypes")
	Query queryObject) {
		if (values != null && values.length > 0)
			for (int i = 0; i < values.length; i++)
				queryObject.setParameter("param" + (i + 1), values[i]);

		if (max != null && max > 0)
			queryObject.setMaxResults(max);
	}

	/**
	 * Utility method useful for preparing an Hibernate query for generic result
	 * 
	 * @param expression The expression for the query
	 * @param values The parameters values to be used (optional, if the query is
	 *        parametric)
	 * @param max Optional maximum number of wanted results
	 * 
	 * @return The Hibernate query
	 */
	protected Query<Object[]> prepareQuery(String expression, Map<String, Object> values, Integer max) {
		Query<Object[]> queryObject = sessionFactory.getCurrentSession().createQuery(expression, Object[].class);
		applyParamsAndLimit(values, max, queryObject);
		return queryObject;
	}

	/**
	 * Utility method useful for preparing an Hibernate query for objects of
	 * this type
	 * 
	 * @param expression The expression for the query
	 * @param values The parameters values to be used (optional, if the query is
	 *        parametric)
	 * @param max Optional maximum number of wanted results
	 * 
	 * @return The Hibernate query
	 */
	protected Query<T> prepareQueryForObject(String expression, Map<String, Object> values, Integer max) {
		Query<T> queryObject = sessionFactory.getCurrentSession().createQuery(expression, entityClass);
		applyParamsAndLimit(values, max, queryObject);
		return queryObject;
	}

	/**
	 * Utility method useful for preparing an Hibernate query for updates
	 * 
	 * @param expression The expression for the query
	 * @param values The parameters values to be used (optional, if the query is
	 *        parametric)
	 * @param max Optional maximum number of wanted results
	 * 
	 * @return The Hibernate query
	 */
	@SuppressWarnings("rawtypes")
	protected Query prepareQueryForUpdate(String expression, Map<String, Object> values, Integer max) {
		Query queryObject = sessionFactory.getCurrentSession().createQuery(expression);
		applyParamsAndLimit(values, max, queryObject);
		return queryObject;
	}

	private void applyParamsAndLimit(Map<String, Object> values, Integer max, @SuppressWarnings("rawtypes")
	Query queryObject) {
		if (values != null)
			for (String name : values.keySet())
				queryObject.setParameter(name, values.get(name));

		if (max != null && max > 0)
			queryObject.setMaxResults(max);
	}

	/**
	 * Doesn't do anything by default
	 */
	@Override
	public void initialize(T entity) {
		// By default do nothing
	}

	protected Session getCurrentSession() {
		return sessionFactory.getCurrentSession();
	}

	/**
	 * Parses a SQL query and inserts the hits to the SQL processor to restrict
	 * the maximum number of returned records. The syntax varies depending on
	 * the current DBMS.
	 * 
	 * @param srcQuery The source query to parse
	 * @param maxRows Max number of rows.
	 * @return The modified qery
	 */
	private String insertTopClause(String srcQuery, Integer maxRows) {
		if (maxRows == null || maxRows.intValue() <= 0)
			return srcQuery;

		String outQuery = srcQuery;
		if (isMySQL() || isPostgreSQL()) {
			/*
			 * At the end of the query we have to insert the LIMIT clause:
			 * 
			 * SELECT column_name(s) FROM table_name WHERE condition LIMIT
			 * number;
			 */
			if (srcQuery.endsWith(";"))
				outQuery = srcQuery.substring(0, srcQuery.length() - 1);
			outQuery += " LIMIT " + maxRows;
		} else if (isSqlServer()) {
			/*
			 * After the SELECT have to put the TOP clause:
			 * 
			 * SELECT TOP number column_name(s) FROM table_name WHERE condition;
			 */
			if (srcQuery.startsWith("SELECT"))
				outQuery = outQuery.replaceFirst("SELECT", "SELECT TOP " + maxRows + " ");
			else if (srcQuery.startsWith("select"))
				outQuery = outQuery.replaceFirst("select", "select TOP " + maxRows + " ");
		} else if (isOracle()) {
			/*
			 * In the WHERE we have to put the ROWNUM condition:
			 * 
			 * SELECT column_name(s) FROM table_name WHERE ROWNUM <= number;
			 */
			if (srcQuery.contains("WHERE"))
				outQuery = outQuery.replaceFirst("WHERE", "where ROWNUM <= " + maxRows + " and ");
			if (srcQuery.contains("where"))
				outQuery = outQuery.replaceFirst("where", "where ROWNUM <= " + maxRows + " and ");
		}

		return outQuery;
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public List query(String sql, Object[] args, RowMapper rowMapper, Integer maxRows) throws PersistenceException {
		List list = new ArrayList();
		try {
			DataSource dataSource = (DataSource) Context.get().getBean(DATA_SOURCE);

			JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
			if (maxRows != null)
				jdbcTemplate.setMaxRows(maxRows);
			if (args != null)
				list = jdbcTemplate.query(insertTopClause(sql, maxRows), args, rowMapper);
			else
				list = jdbcTemplate.query(insertTopClause(sql, maxRows), rowMapper);
			return list;
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}

	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public List queryForList(String sql, Object[] args, Class elementType, Integer maxRows)
			throws PersistenceException {

		List list = new ArrayList();
		try {
			DataSource dataSource = (DataSource) Context.get().getBean(DATA_SOURCE);
			JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
			if (maxRows != null)
				jdbcTemplate.setMaxRows(maxRows);
			if (args != null)
				list = jdbcTemplate.queryForList(insertTopClause(sql, maxRows), args, elementType);
			else
				list = jdbcTemplate.queryForList(insertTopClause(sql, maxRows), elementType);
			return list;
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw new PersistenceException(e);
		}
	}

	@Override
	public SqlRowSet queryForRowSet(String sql, Object[] args, Integer maxRows) throws PersistenceException {
		try {
			SqlRowSet rs = null;
			DataSource dataSource = (DataSource) Context.get().getBean(DATA_SOURCE);
			JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
			if (maxRows != null)
				jdbcTemplate.setMaxRows(maxRows);
			if (args != null)
				rs = jdbcTemplate.queryForRowSet(insertTopClause(sql, maxRows), args);
			else
				rs = jdbcTemplate.queryForRowSet(insertTopClause(sql, maxRows));
			return new SqlRowSetWrapper(rs);
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@SuppressWarnings("rawtypes")
	@Override
	public List queryForList(String sql, Class elementType) throws PersistenceException {
		return queryForList(sql, null, elementType, null);
	}

	@Override
	public int queryForInt(String sql) throws PersistenceException {
		return (int) queryForLong(sql);
	}

	@Override
	public long queryForLong(String sql) throws PersistenceException {
		try {
			DataSource dataSource = (DataSource) Context.get().getBean(DATA_SOURCE);
			JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
			return jdbcTemplate.queryForObject(sql, Long.class);
		} catch (NullPointerException | EmptyResultDataAccessException e) {
			return 0L;
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@Override
	public long queryForLong(String sql, Object... args) throws PersistenceException {
		try {
			DataSource dataSource = (DataSource) Context.get().getBean(DATA_SOURCE);
			JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
			return jdbcTemplate.queryForObject(sql, Long.class, args);
		} catch (NullPointerException | EmptyResultDataAccessException e) {
			return 0L;
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@Override
	public String queryForString(String sql) throws PersistenceException {
		try {
			DataSource dataSource = (DataSource) Context.get().getBean(DATA_SOURCE);
			JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
			return jdbcTemplate.queryForObject(sql, String.class);
		} catch (EmptyResultDataAccessException e) {
			return null;
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
			throw new PersistenceException(e);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public Object queryForObject(String sql, @SuppressWarnings("rawtypes")
	Class type) throws PersistenceException {
		try {
			DataSource dataSource = (DataSource) Context.get().getBean(DATA_SOURCE);
			JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
			return jdbcTemplate.queryForObject(sql, type);
		} catch (EmptyResultDataAccessException e) {
			return null;
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@Override
	public int jdbcUpdate(String statement) throws PersistenceException {
		if (!checkStoringAspect())
			return 0;

		try {
			DataSource dataSource = (DataSource) Context.get().getBean(DATA_SOURCE);
			JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
			return jdbcTemplate.update(statement);
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@Override
	public void deleteAll(Collection<T> entities, int code) throws PersistenceException {
		if (!checkStoringAspect())
			return;

		if (entities == null || entities.isEmpty())
			return;

		try {
			StringBuilder ids = new StringBuilder();
			for (T t : entities) {
				if (ids.length() > 0)
					ids.append(",");
				ids.append(Long.toString(t.getId()));
			}

			Query<T> queryObject = prepareQueryForObject(UPDATE + entityClass.getCanonicalName() + " set deleted="
					+ code + " where id in(" + ids.toString() + ")", (Map<String, Object>) null, null);
			queryObject.executeUpdate();
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@Override
	public void deleteAll(Collection<T> entities) throws PersistenceException {
		deleteAll(entities, PersistentObject.DELETED_CODE_DEFAULT);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public int bulkUpdate(String expression, Object[] values) throws PersistenceException {
		if (!checkStoringAspect())
			return 0;

		try {
			Query queryObject = prepareQueryForUpdate(UPDATE + entityClass.getCanonicalName() + " " + expression,
					values, null);
			return queryObject.executeUpdate();
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@Override
	public int bulkUpdate(String expression, Map<String, Object> parameters) throws PersistenceException {
		if (!checkStoringAspect())
			return 0;

		try {
			Query queryObject = prepareQueryForUpdate(UPDATE + entityClass.getCanonicalName() + " " + expression,
					parameters, null);
			return queryObject.executeUpdate();
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	@Override
	public int jdbcUpdate(String statement, Object... args) throws PersistenceException {
		if (!checkStoringAspect())
			return 0;

		DataSource dataSource = (DataSource) Context.get().getBean(DATA_SOURCE);
		try {
			JdbcTemplate jdbcTemplate = new JdbcTemplate(dataSource);
			return jdbcTemplate.update(statement, args);
		} catch (Throwable e) {
			throw new PersistenceException(e);
		}
	}

	protected Connection getConnection() throws SQLException {
		DataSource dataSource = (DataSource) Context.get().getBean(DATA_SOURCE);
		return dataSource.getConnection();
	}

	@Override
	public String getDbms() {
		ContextProperties config = Context.get().getProperties();
		return config.getProperty("jdbc.dbms", "mysql").toLowerCase();
	}

	@Override
	public boolean isOracle() {
		return "oracle".equals(getDbms());
	}

	protected boolean isHsql() {
		return "hsqldb".equals(getDbms());
	}

	protected boolean isMySQL() {
		return "mysql".equals(getDbms());
	}

	protected boolean isMariaDB() {
		return "maria".equals(getDbms());
	}

	protected boolean isPostgreSQL() {
		return "postgresql".equals(getDbms());
	}

	protected boolean isSqlServer() {
		return "mssql".equals(getDbms());
	}

	public SessionFactory getSessionFactory() {
		return sessionFactory;
	}

	/**
	 * A generic method to logically delete old records of a table, useful for
	 * some DAO implementations. This method uses JDBC directly. The processed
	 * table must provide the ld_deleted and ld_lastmodified columns. The
	 * dateColumn is the column used to validate the ttl
	 * 
	 * 
	 * @param ttl number of retention days
	 * @param tableName name of the table to process
	 * @param dateColumn
	 * 
	 * @throws PersistenceException error at database level
	 */
	protected int cleanOldRecords(int ttl, String tableName, String dateColumn) throws PersistenceException {
		int updates = 0;
		if (ttl > 0) {
			Date today = new Date();
			GregorianCalendar cal = new GregorianCalendar();
			cal.add(Calendar.DAY_OF_MONTH, -ttl);
			Date ldDate = cal.getTime();

			updates = jdbcUpdate("UPDATE " + tableName + " SET ld_deleted = 1, ld_lastmodified = ?"
					+ " WHERE ld_deleted = 0 AND " + dateColumn + " < ?", today, ldDate);

			log.info("Removed {} old rows from table {}: ", updates, tableName);
		}
		return updates;
	}

	/**
	 * A short cut for {@link #cleanOldRecords(int, String, String)} that passes
	 * the dateColumn="ld_date"
	 * 
	 * 
	 * @param ttl number of retention days
	 * @param tableName name of the table to process
	 * 
	 * @throws PersistenceException error at database level
	 */
	protected int cleanOldRecords(int ttl, String tableName) throws PersistenceException {
		return cleanOldRecords(ttl, tableName, "ld_date");
	}
}