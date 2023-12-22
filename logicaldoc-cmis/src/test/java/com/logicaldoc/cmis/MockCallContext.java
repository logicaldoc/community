package com.logicaldoc.cmis;

import java.io.File;
import java.math.BigInteger;

import org.apache.chemistry.opencmis.commons.enums.CmisVersion;
import org.apache.chemistry.opencmis.commons.server.CallContext;

/**
 * A simple CallContext jsut to do some tests
 * 
 * @author Francesco Bignardi
 * @since 8.9
 */
public class MockCallContext implements CallContext {

	private String sid;

	public String getSid() {
		return sid;
	}

	private String repositoryId;

	private String username = "admin";

	public void setUsername(String username) {
		this.username = username;
	}

	public MockCallContext(String sid, String repositoryId) {
		super();
		this.sid = sid;
		this.repositoryId = repositoryId;
	}

	@Override
	public boolean encryptTempFiles() {

		return false;
	}

	@Override
	public Object get(String arg0) {

		return null;
	}

	@Override
	public String getBinding() {

		return null;
	}

	@Override
	public CmisVersion getCmisVersion() {

		return null;
	}

	@Override
	public BigInteger getLength() {

		return null;
	}

	@Override
	public String getLocale() {

		return null;
	}

	@Override
	public long getMaxContentSize() {

		return 0;
	}

	@Override
	public int getMemoryThreshold() {

		return 0;
	}

	@Override
	public BigInteger getOffset() {
		return null;
	}

	@Override
	public String getPassword() {

		return null;
	}

	@Override
	public String getRepositoryId() {
		return repositoryId;
	}

	@Override
	public File getTempDirectory() {
		return new File("target");
	}

	@Override
	public String getUsername() {
		return "admin";
	}

	@Override
	public boolean isObjectInfoRequired() {
		return true;
	}

}
