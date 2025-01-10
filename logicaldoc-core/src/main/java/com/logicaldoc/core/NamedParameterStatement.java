package com.logicaldoc.core;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * This class wraps around a {@link PreparedStatement} and allows the programmer
 * to set parameters by name instead of by index. This eliminates any confusion
 * as to which parameter index represents what. This also means that rearranging
 * the SQL statement or adding a parameter doesn't involve renumbering your
 * indices. Code such as this:
 *
 * 
 * Connection con=getConnection(); String query="select * from my_table where
 * name=? or address=?"; PreparedStatement p=con.prepareStatement(query);
 * p.setString(1, "bob"); p.setString(2, "123 terrace ct"); ResultSet
 * rs=p.executeQuery();
 *
 * 
 * can be replaced with:
 *
 * 
 * Connection con=getConnection(); String query="select * from my_table where
 * name=:name or address=:address"; NamedParameterStatement p=new
 * NamedParameterStatement(con, query); p.setString("name", "bob");
 * p.setString("address", "123 terrace ct"); ResultSet rs=p.executeQuery();
 * 
 */
public class NamedParameterStatement implements AutoCloseable {

	/** The statement this object is wrapping. */
	private final PreparedStatement statement;

	/**
	 * Maps parameter names to arrays of ints which are the parameter indices.
	 */
	private final Map<String, List<Integer>> indexMap;

	/**
	 * Creates a NamedParameterStatement. Wraps a call to
	 * <code>c.{@link Connection#prepareStatement(java.lang.String) prepareStatement}</code>.
	 * 
	 * @param connection the database connection
	 * @param query the parameterized query
	 * @param parameters the map of parameter values
	 * 
	 * @throws SQLException if the statement could not be created
	 */
	public NamedParameterStatement(Connection connection, String query, Map<String, Object> parameters)
			throws SQLException {
		this(connection, query, parameters, null);
	}

	/**
	 * Creates a NamedParameterStatement. Wraps a call to
	 * <code>c.{@link Connection#prepareStatement(java.lang.String) prepareStatement}</code>.
	 * 
	 * @param connection the database connection
	 * @param query the parameterized query
	 * @param parameters the map of parameter values
	 * @param maxRows maximum number of records to return
	 * 
	 * @throws SQLException if the statement could not be created
	 */
	public NamedParameterStatement(Connection connection, String query, Map<String, Object> parameters, Integer maxRows)
			throws SQLException {
		indexMap = new HashMap<>();

		// remove those parameters not really referenced in the query
		Map<String, Object> usedParameters = new HashMap<>();
		if (parameters != null)
			for (Map.Entry<String, Object> entry : parameters.entrySet()) {
				if (query.contains(":" + entry.getKey()))
					usedParameters.put(entry.getKey(), entry.getValue());
			}

		String parsedQuery = parse(query, indexMap);
		statement = connection.prepareStatement(parsedQuery);
		if (!usedParameters.isEmpty()) {
			setParameters(usedParameters);
		}
		if (maxRows != null)
			statement.setMaxRows(maxRows);
	}

	/**
	 * Parses a query with named parameters. The parameter-index mappings are
	 * put into the map, and the parsed query is returned.
	 * 
	 * @param query query to parse
	 * @param paramMap map to hold parameter-index mappings
	 * @return the parsed query
	 */
	private static final String parse(String query, Map<String, List<Integer>> paramMap) {
		// I was originally using regular expressions, but they didn't work well
		// for ignoring parameter-like strings inside quotes.
		int length = query.length();
		StringBuilder parsedQuery = new StringBuilder(length);
		boolean inSingleQuote = false;
		boolean inDoubleQuote = false;
		int index = 1;

		int i = 0;
		while (i < length) {
			char c = query.charAt(i);
			if (inSingleQuote && c == '\'') {
				inSingleQuote = false;
			} else if (inDoubleQuote && c == '"') {
				inDoubleQuote = false;
			} else {
				if (c == '\'') {
					inSingleQuote = true;
				} else if (c == '"') {
					inDoubleQuote = true;
				} else if (c == ':' && i + 1 < length && Character.isJavaIdentifierStart(query.charAt(i + 1))) {
					String name = getParameterName(query, i);
					c = '?'; // replace the parameter with a question mark
					i += name.length(); // skip past the end if the parameter

					paramMap.putIfAbsent(name, new LinkedList<>());
					paramMap.get(name).add(Integer.valueOf(index++));
				}
			}
			parsedQuery.append(c);
			i++;
		}

		return parsedQuery.toString();

	}

	private static String getParameterName(String query, int currentPosition) {
		int j = currentPosition + 2;
		while (j < query.length() && Character.isJavaIdentifierPart(query.charAt(j)))
			j++;
		return query.substring(currentPosition + 1, j);
	}

	/**
	 * Returns the indexes for a parameter.
	 * 
	 * @param name parameter name
	 * @return parameter indexes
	 * @throws IllegalArgumentException if the parameter does not exist
	 */
	private Integer[] getIndexes(String name) {
		Integer[] indexes = indexMap.get(name).toArray(new Integer[0]);
		if (indexes == null)
			throw new IllegalArgumentException("Parameter not found: " + name);
		return indexes;
	}

	public void setParameters(Map<String, Object> parameters) throws SQLException {
		for (Map.Entry<String, Object> entry : parameters.entrySet()) {
			if (entry.getValue() instanceof String str)
				setString(entry.getKey(), str);
			else if (entry.getValue() instanceof Integer intg)
				setInt(entry.getKey(), intg);
			else if (entry.getValue() instanceof Long lng)
				setLong(entry.getKey(), lng);
			else if (entry.getValue() instanceof Timestamp timestamp)
				setTimestamp(entry.getKey(), timestamp);
			else if (entry.getValue() instanceof Date date)
				setTimestamp(entry.getKey(), new Timestamp(date.getTime()));
			else
				setObject(entry.getKey(), entry.getValue());
		}
	}

	/**
	 * Sets a parameter.
	 * 
	 * @param name parameter name
	 * @param value parameter value
	 * @throws SQLException if an error occurred
	 * @throws IllegalArgumentException if the parameter does not exist
	 * @see PreparedStatement#setObject(int, java.lang.Object)
	 */
	public void setObject(String name, Object value) throws SQLException {
		Integer[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setObject(indexes[i], value);
		}
	}

	/**
	 * Sets a parameter.
	 * 
	 * @param name parameter name
	 * @param value parameter value
	 * @throws SQLException if an error occurred
	 * @throws IllegalArgumentException if the parameter does not exist
	 * @see PreparedStatement#setString(int, java.lang.String)
	 */
	public void setString(String name, String value) throws SQLException {
		Integer[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setString(indexes[i], value);
		}
	}

	/**
	 * Sets a parameter.
	 * 
	 * @param name parameter name
	 * @param value parameter value
	 * @throws SQLException if an error occurred
	 * @throws IllegalArgumentException if the parameter does not exist
	 * @see PreparedStatement#setInt(int, int)
	 */
	public void setInt(String name, int value) throws SQLException {
		Integer[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setInt(indexes[i], value);
		}
	}

	/**
	 * Sets a parameter.
	 * 
	 * @param name parameter name
	 * @param value parameter value
	 * @throws SQLException if an error occurred
	 * @throws IllegalArgumentException if the parameter does not exist
	 * @see PreparedStatement#setInt(int, int)
	 */
	public void setLong(String name, long value) throws SQLException {
		Integer[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setLong(indexes[i], value);
		}
	}

	/**
	 * Sets a parameter.
	 * 
	 * @param name parameter name
	 * @param value parameter value
	 * @throws SQLException if an error occurred
	 * @throws IllegalArgumentException if the parameter does not exist
	 * @see PreparedStatement#setTimestamp(int, java.sql.Timestamp)
	 */
	public void setTimestamp(String name, Timestamp value) throws SQLException {
		Integer[] indexes = getIndexes(name);
		for (int i = 0; i < indexes.length; i++) {
			statement.setTimestamp(indexes[i], value);
		}
	}

	/**
	 * Returns the underlying statement.
	 * 
	 * @return the statement
	 */
	public PreparedStatement getStatement() {
		return statement;
	}

	/**
	 * Executes the statement.
	 * 
	 * @return true if the first result is a {@link ResultSet}
	 * @throws SQLException if an error occurred
	 * @see PreparedStatement#execute()
	 */
	public boolean execute() throws SQLException {
		return statement.execute();
	}

	/**
	 * Executes the statement, which must be a query.
	 * 
	 * @return the query results
	 * @throws SQLException if an error occurred
	 * @see PreparedStatement#executeQuery()
	 */
	public ResultSet executeQuery() throws SQLException {
		return statement.executeQuery();
	}

	/**
	 * Executes the statement, which must be an SQL INSERT, UPDATE or DELETE
	 * statement; or an SQL statement that returns nothing, such as a DDL
	 * statement.
	 * 
	 * @return number of rows affected
	 * @throws SQLException if an error occurred
	 * @see PreparedStatement#executeUpdate()
	 */
	public int executeUpdate() throws SQLException {
		return statement.executeUpdate();
	}

	/**
	 * Closes the statement.
	 * 
	 * @throws SQLException if an error occurred
	 * @see Statement#close()
	 */
	@Override
	public void close() throws SQLException {
		statement.close();
	}

	/**
	 * Adds the current set of parameters as a batch entry.
	 * 
	 * @throws SQLException if something went wrong
	 */
	public void addBatch() throws SQLException {
		statement.addBatch();
	}

	/**
	 * Executes all of the batched statements.
	 * 
	 * See {@link Statement#executeBatch()} for details.
	 * 
	 * @return update counts for each statement
	 * @throws SQLException if something went wrong
	 */
	public int[] executeBatch() throws SQLException {
		return statement.executeBatch();
	}
}
