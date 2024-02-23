package com.logicaldoc.util.sql;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Date;

/**
 * This class contains methods about SQL handling
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 4.5
 */
public class SqlUtil {

	private SqlUtil() {
	}

	/**
	 * Double the quotes in a string. This means that each character ' will be
	 * replaced with '' (two quote characters)
	 * 
	 * @param input The incoming string
	 * @return A string with doubled quotes
	 */
	public static String doubleQuotes(String input) {
		if (input == null)
			return "";
		return input.replace("'", "''");
	}

	/**
	 * Double the backslashes in a string. This means that each character \ will
	 * be replaced with \\
	 * 
	 * @param input The incoming string
	 * @return A string with doubled backslashes
	 */
	public static String doubleBackslashes(String input) {
		if (input == null)
			return "";
		return input.replace("\\", "\\\\");
	}

	public static String doubleQuotesAndBackslashes(String input) {
		return doubleBackslashes(doubleQuotes(input));
	}

	/**
	 * Retrieves the date stored in a given column
	 * 
	 * @param resultSet The resultset to use
	 * @param column The column index
	 * @return The date value
	 * 
	 * @throws SQLException Error in the database
	 */
	public static Date getColumnDateValue(ResultSet resultSet, int column) throws SQLException {
		Date date;
		if (resultSet.getObject(column) instanceof Timestamp)
			date = resultSet.getTimestamp(column);
		else if (resultSet.getObject(column) instanceof LocalDateTime localDateTime)
			date = java.sql.Timestamp.valueOf(localDateTime);
		else
			date = resultSet.getDate(column);
		return date;
	}
}