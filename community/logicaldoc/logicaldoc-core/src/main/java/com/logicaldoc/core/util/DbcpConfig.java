package com.logicaldoc.core.util;

/**
 * Configuration bean for DBCP
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2
 */
public class DbcpConfig {
	
	private String driverClassName;

	private int maxTotal;

	private int maxIdle;

	private int initialSize;

	private boolean testOnBorrow;

	private long timeBetweenEvictionRunsMillis;

	private int numTestsPerEvictionRun;

	private boolean testWhileIdle;

	private volatile String password;

	private String url;

	private String username;

	private volatile String validationQuery;
	
	private String connectionInitSqls;

	public String getDriverClassName() {
		return driverClassName;
	}

	public int getMaxTotal() {
		return maxTotal;
	}

	public int getMaxIdle() {
		return maxIdle;
	}

	public int getInitialSize() {
		return initialSize;
	}

	public boolean isTestOnBorrow() {
		return testOnBorrow;
	}

	public long getTimeBetweenEvictionRunsMillis() {
		return timeBetweenEvictionRunsMillis;
	}

	public int getNumTestsPerEvictionRun() {
		return numTestsPerEvictionRun;
	}

	public boolean isTestWhileIdle() {
		return testWhileIdle;
	}

	public java.lang.String getPassword() {
		return password;
	}

	public java.lang.String getUrl() {
		return url;
	}

	public java.lang.String getValidationQuery() {
		return validationQuery;
	}

	public void setDriverClassName(java.lang.String driverClassName) {
		this.driverClassName = driverClassName;
	}

	public void setMaxTotal(int maxTotal) {
		this.maxTotal = maxTotal;
	}

	public void setMaxIdle(int maxIdle) {
		this.maxIdle = maxIdle;
	}

	public void setInitialSize(int initialSize) {
		this.initialSize = initialSize;
	}

	public void setTestOnBorrow(boolean testOnBorrow) {
		this.testOnBorrow = testOnBorrow;
	}

	public void setTimeBetweenEvictionRunsMillis(long timeBetweenEvictionRunsMillis) {
		this.timeBetweenEvictionRunsMillis = timeBetweenEvictionRunsMillis;
	}

	public void setNumTestsPerEvictionRun(int numTestsPerEvictionRun) {
		this.numTestsPerEvictionRun = numTestsPerEvictionRun;
	}

	public void setTestWhileIdle(boolean testWhileIdle) {
		this.testWhileIdle = testWhileIdle;
	}

	public void setPassword(java.lang.String password) {
		this.password = password;
	}

	public void setUrl(java.lang.String url) {
		this.url = url;
	}

	public void setValidationQuery(java.lang.String validationQuery) {
		this.validationQuery = validationQuery;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getConnectionInitSqls() {
		return connectionInitSqls;
	}

	public void setConnectionInitSqls(String connectionInitSqls) {
		this.connectionInitSqls = connectionInitSqls;
	}
}