package com.logicaldoc.util.spring;

import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.PropertyPlaceholderConfigurer;

import com.logicaldoc.util.config.ContextProperties;

/**
 * Extends the standard property placeholder to retrieve properties from the
 * database.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 1.0
 */
public class PropertiesPlaceHolder extends PropertyPlaceholderConfigurer {
	private static Logger log = LoggerFactory.getLogger(PropertiesPlaceHolder.class);

	@Override
	public Properties mergeProperties() throws IOException {
		return super.mergeProperties();
	}

	@Override
	protected String resolvePlaceholder(String placeholder, Properties props) {
		if (placeholder.startsWith("jdbc."))
			return super.resolvePlaceholder(placeholder, props);
		String value = getDbProperty(placeholder);
		if (value != null)
			return value;
		else
			return props.getProperty(placeholder);
	}

	/**
	 * Tries to retrieve a property by using a direct JDBC query against the DB
	 * 
	 * @param property The property name
	 * @return The found value, may be null
	 */
	private String getDbProperty(String property) {

		Connection conn = null;
		Statement stmt = null;
		PreparedStatement pstmt = null;
		ResultSet rs = null;
		try {
			ContextProperties conf = new ContextProperties();

			// Load the database driver
			Class.forName(conf.getProperty("jdbc.driver"));

			// Get a connection to the database
			conn = DriverManager.getConnection(conf.getProperty("jdbc.url"), conf.getProperty("jdbc.username"),
					conf.getProperty("jdbc.password"));

			// Get a statement from the connection
			stmt = conn.createStatement();

			// Execute the query
			String query = "select ld_string1 from ld_generic where ld_deleted=0 and ld_type='conf' and ld_subtype=?";
			pstmt = conn.prepareStatement(query);
			pstmt.setString(1, property);
			rs = pstmt.executeQuery();

			// Loop through the result set
			while (rs.next())
				return rs.getString(1);

			return null;
		} catch (Throwable se) {
			log.error(se.getMessage());
			return null;
		} finally {
			try {
				if (rs != null)
					rs.close();
			} catch (Throwable se) {
			}
			try {
				if (stmt != null)
					stmt.close();
			} catch (Throwable se) {
			}
			try {
				if (pstmt != null)
					pstmt.close();
			} catch (Throwable se) {
			}
			try {
				if (conn != null)
					conn.close();
			} catch (Throwable se) {
			}
		}
	}
}
