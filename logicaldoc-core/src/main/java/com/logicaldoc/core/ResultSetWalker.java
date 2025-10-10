package com.logicaldoc.core;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * A walker that operates over a {@link ResultSet} bound to the current JDBC connection
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.0.1
 */
public interface ResultSetWalker {

	/**
	 * Implement in this method your iteration logic
	 * 
	 * @param rows The {@link ResultSet} ready to be iterated
	 * 
	 * @throws SQLException Error in the database
	 */
	public void walk(ResultSet rows) throws SQLException;
}
