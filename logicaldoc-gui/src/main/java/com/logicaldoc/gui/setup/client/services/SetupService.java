package com.logicaldoc.gui.setup.client.services;

import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.setup.client.SetupInfo;

/**
 * The client side stub for the Setup Service. This service allows the
 * installation of an instance of LogicalDOC.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
@RemoteServiceRelativePath("setup")
public interface SetupService extends RemoteService {

	/**
	 * Performs simple security check to enforce that only the admin can go
	 * through the setup
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void securityCheck() throws ServerException;

	/**
	 * Performs a system setup.
	 * 
	 * @param data The intallation data
	 * 
	 * @throws ServerException an error happened in the server applications
	 */
	public void setup(SetupInfo data) throws ServerException;
}
