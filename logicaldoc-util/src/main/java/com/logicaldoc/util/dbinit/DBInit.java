package com.logicaldoc.util.dbinit;

import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.hsqldb.cmdline.SqlFile;
import org.hsqldb.cmdline.SqlToolError;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.io.FileUtil;

/**
 * Database initialisation utility
 * 
 * @author Michael Scholz
 * @author Marco Meschieri
 */
public class DBInit {

	protected static Logger log = LoggerFactory.getLogger(DBInit.class);

	private String dbms = "";

	private String driver = "";

	private String url = "";

	private String username = "";

	private String password = System.getProperty("database.password");

	private Connection con;

	// List of sql files to be executed
	private List<String> sqlList = new ArrayList<>();

	/**
	 * A list of sql files to execute
	 * 
	 * @param sqlList The list of sql files
	 */
	public DBInit(List<String> sqlList) {
		this.sqlList = sqlList;
	}

	public DBInit() {
	}

	/**
	 * Executes all the sql files defined in the constructor
	 */
	public void execute() {
		try {
			doConnection();
			for (String sql : sqlList)
				execute(sql);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			try {
				if (con != null)
					con.close();
			} catch (SQLException e1) {
				log.error(e1.getMessage(), e1);
			}
		}
	}

	/**
	 * Execute a SQL statements in the passed string
	 * 
	 * @param sql The SQL to execute
	 */
	public void executeSql(String sql) {
		try {
			doConnection();
			try (PreparedStatement st = con.prepareStatement(sql)) {
				st.execute();
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
			log.error("Failed to execute " + sql, e);
			try {
				if (con != null)
					con.close();
			} catch (SQLException e1) {
				log.error(e1.getMessage(), e1);
			}
		}
	}

	/**
	 * Executes a single sql file
	 * 
	 * @param sqlFile Path of the file to execute(it can be a classpath
	 *        resource)
	 * @throws IOException
	 * @throws SqlToolError
	 * @throws SQLException
	 */
	private void execute(String sqlFile) throws IOException, SQLException {

		log.debug("Execute {}", sqlFile);
		
		Logger console = LoggerFactory.getLogger("console");
		console.info("Execute {}", sqlFile);

		File file = new File(sqlFile);
		if (!file.exists() || !file.canRead()) {
			file = FileUtil.createTempFile(file.getName(), ".sql");
			file.deleteOnExit();
			FileUtil.copyResource(sqlFile, file);
		}

		SqlFile sFile = new SqlFile(file, "Cp1252", false);
		try {
			sFile.setContinueOnError(true);
			sFile.setConnection(con);
			sFile.execute();
		} catch (SqlToolError e) {
			throw new SQLException(e.getMessage());
		}
	}

	protected void doConnection() {
		try {
			Class.forName(driver);
			con = DriverManager.getConnection(url, username, password);
			con.setAutoCommit(true);
		} catch (Exception ex) {
			log.error(ex.getMessage(), ex);
		}
	}

	protected void rollback() {
		try {
			con.rollback();
		} catch (Exception ex) {
			log.error("can't rollback", ex);
		}
	}

	/**
	 * This method returns the state of the connection to the database.
	 * 
	 * @return checks true if the database is up and running and well connected
	 */
	public boolean isConnected() {
		try {
			return !con.isClosed();
		} catch (Exception ex) {
			log.debug("db-connection is open: {}", ex.getMessage(), ex);
			return false;
		}
	}

	/**
	 * This method tests a connection to the database
	 * 
	 * @return true if the database can be connected
	 */
	public boolean testConnection() {

		boolean result = false;

		try {
			Class.forName(driver);
			con = DriverManager.getConnection(url, username, password);
			result = true;
			con.close();
		} catch (Exception ex) {
			log.error(ex.getMessage(), ex);
		}

		return result;
	}

	public void setDriver(String driver) {
		this.driver = driver;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public List<String> getSqlList() {
		return sqlList;
	}

	public String getDbms() {
		return dbms;
	}

	public void setDbms(String dbms) {
		this.dbms = dbms;
	}
}