package com.logicaldoc.core.util;

import java.io.Closeable;
import java.io.IOException;
import java.io.PrintWriter;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.time.Duration;
import java.util.Arrays;

import javax.sql.DataSource;

import org.apache.commons.dbcp2.BasicDataSource;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

/**
 * A configurable connection pool that wraps different technologies, you decide
 * the implementation through the implementation setting: hikari or dbcp
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2
 */
public class ConnectionPool implements DataSource, Closeable {

	private static final Logger log = LoggerFactory.getLogger(ConnectionPool.class);

	private DataSource wrappedDataSource;

	private HikariConfig hikariConfig;

	private DbcpConfiguration dbcpConfig;

	private String implementation;

	public DataSource getWrappedDataSource() {
		if (wrappedDataSource == null) {
			log.info("Instantiating connection pool {}", implementation);
			if ("dbcp".equals(implementation)) {
				BasicDataSource ds = new BasicDataSource();
				ds.setMaxTotal(dbcpConfig.getMaxTotal());
				ds.setMaxIdle(dbcpConfig.getMaxIdle());
				ds.setInitialSize(dbcpConfig.getInitialSize());
				ds.setTestOnBorrow(dbcpConfig.isTestOnBorrow());
				ds.setDurationBetweenEvictionRuns(Duration.ofMillis(dbcpConfig.getTimeBetweenEvictionRunsMillis()));
				ds.setNumTestsPerEvictionRun(dbcpConfig.getNumTestsPerEvictionRun());
				ds.setTestWhileIdle(dbcpConfig.isTestWhileIdle());
				ds.setPassword(dbcpConfig.getPassword());
				ds.setUrl(dbcpConfig.getUrl());
				ds.setUsername(dbcpConfig.getUsername());
				ds.setValidationQuery(dbcpConfig.getValidationQuery());

				if (StringUtils.isNotEmpty(dbcpConfig.getConnectionInitSqls())) {
					String[] sqls = dbcpConfig.getConnectionInitSqls().split(";");
					ds.setConnectionInitSqls(Arrays.asList(sqls));
				}

				wrappedDataSource = ds;
			} else {
				wrappedDataSource = new HikariDataSource(hikariConfig);
			}
		}
		return wrappedDataSource;
	}

	public void setHikariConfig(HikariConfig hikaryConfig) {
		this.hikariConfig = hikaryConfig;
	}

	public PrintWriter getLogWriter() throws SQLException {
		return getWrappedDataSource().getLogWriter();
	}

	public <T> T unwrap(Class<T> iface) throws SQLException {
		return getWrappedDataSource().unwrap(iface);
	}

	public void setLogWriter(PrintWriter out) throws SQLException {
		getWrappedDataSource().setLogWriter(out);
	}

	public boolean isWrapperFor(Class<?> iface) throws SQLException {
		return getWrappedDataSource().isWrapperFor(iface);
	}

	public Connection getConnection() throws SQLException {
		return getWrappedDataSource().getConnection();
	}

	public void setLoginTimeout(int seconds) throws SQLException {
		getWrappedDataSource().setLoginTimeout(seconds);
	}

	public Connection getConnection(String username, String password) throws SQLException {
		return getWrappedDataSource().getConnection(username, password);
	}

	public int getLoginTimeout() throws SQLException {
		return getWrappedDataSource().getLoginTimeout();
	}

	public java.util.logging.Logger getParentLogger() throws SQLFeatureNotSupportedException {
		return getWrappedDataSource().getParentLogger();
	}

	public void init() {
		getWrappedDataSource();
	}

	@Override
	public void close() throws IOException {
		if (getWrappedDataSource() instanceof Closeable closeable) {
			closeable.close();
		}
	}

	public void setImplementation(String implementation) {
		this.implementation = implementation;
	}

	public void setDbcpConfig(DbcpConfiguration dbcpConfig) {
		this.dbcpConfig = dbcpConfig;
	}

}