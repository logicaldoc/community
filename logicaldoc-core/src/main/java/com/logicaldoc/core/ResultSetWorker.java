package com.logicaldoc.core;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * A worker that operates in a result set bound to the current JDBC connection
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.0.1
 */
public interface ResultSetWorker {

	/**
	 * Implement in this method your iteration logic
	 * 
	 * @param rows The result set ready to be used
	 * 
	 * @throws SQLException Error in the database
	 */
	public void work(ResultSet rows) throws SQLException;
}
