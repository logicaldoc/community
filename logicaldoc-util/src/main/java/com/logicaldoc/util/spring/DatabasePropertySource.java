package com.logicaldoc.util.spring;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.env.PropertySource;

import com.logicaldoc.util.config.ContextProperties;

/**
 * Reads the propeties from the database (ld_generic of type 'conf')
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.6
 */
public class DatabasePropertySource extends PropertySource<String> {

	public DatabasePropertySource() {
		super("database");
	}

	private static Logger log = LoggerFactory.getLogger(DatabasePropertySource.class);

	/**
	 * Tries to retrieve a property by using a direct JDBC query against the DB
	 * 
	 * @param key The property name
	 * @return The found value, may be null
	 */
	@Override
	public Object getProperty(String key) {
		ContextProperties conf;
		try {
			conf = new ContextProperties();

			// Load the database driver
			if (conf.getProperty("jdbc.driver") != null)
				Class.forName(conf.getProperty("jdbc.driver"));
		} catch (Exception e) {
			log.warn(e.getMessage());
			return null;
		}

		try (Connection conn = DriverManager.getConnection(conf.getProperty("jdbc.url"),
				conf.getProperty("jdbc.username"), conf.getProperty("jdbc.password"));
				Statement stmt = conn.createStatement();) {

			// Execute the query
			String query = "select ld_string1 from ld_generic where ld_deleted=0 and ld_type='conf' and ld_subtype=?";
			try (PreparedStatement pstmt = conn.prepareStatement(query);) {
				pstmt.setString(1, key);
				try (ResultSet rs = pstmt.executeQuery();) {

					// Loop through the result set
					while (rs.next())
						return rs.getString(1);

					return null;
				}
			}
		} catch (SQLException e) {
			log.error(e.getMessage());
			return null;
		}
	}
}
