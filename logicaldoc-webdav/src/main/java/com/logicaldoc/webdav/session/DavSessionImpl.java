package com.logicaldoc.webdav.session;

import java.util.HashMap;

import com.logicaldoc.core.security.Tenant;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.webdav.simple.DavSessionImpl}
 * 
 * @author Sebastian Wenzky
 */
public class DavSessionImpl implements WebdavSession {

	private HashMap<String, Object> map = new HashMap<>();

	private long tenantId = Tenant.DEFAULT_ID;

	/**
	 * @see WebdavSession#getObject(String)
	 */
	@Override
	public Object getObject(String key) {
		return map.get(key);
	}

	/**
	 * @see WebdavSession#putObject(String, Object)
	 */
	@Override
	public void putObject(String key, Object value) {
		map.put(key, value);
	}

	@Override
	public void addLockToken(String arg0) {
		// Nothing to do
	}

	@Override
	public void addReference(Object arg0) {
		throw new UnsupportedOperationException();
	}

	@Override
	public String[] getLockTokens() {
		return new String[] {};
	}

	@Override
	public void removeLockToken(String arg0) {
		throw new UnsupportedOperationException();
	}

	@Override
	public void removeReference(Object arg0) {
		throw new UnsupportedOperationException();
	}

	public long getTenantId() {
		return tenantId;
	}

	public void setTenantId(long tenantId) {
		this.tenantId = tenantId;
	}
}
