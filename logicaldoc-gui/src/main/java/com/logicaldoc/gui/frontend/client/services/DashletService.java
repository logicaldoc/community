package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;

/**
 * The client side stub for the Dashlet Service. This service allows the
 * handling of dashlets.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
@RemoteServiceRelativePath("dashlet")
public interface DashletService extends RemoteService {

	/**
	 * Saves a dashlet
	 * 
	 * @param dashlet the dashlet to save
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void save(GUIDashlet dashlet) throws ServerException;

	/**
	 * Gets a dashlet
	 * 
	 * @param dashletId identifier of the dashlet
	 * 
	 * @return the dashlet retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDashlet get(long dashletId) throws ServerException;

	/**
	 * Saves the dashlet definitions
	 * 
	 * @param dashlets the dashlets to save
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveDashlets(List<GUIDashlet> dashlets) throws ServerException;

	/**
	 * Loads all the dashlet definitions
	 * 
	 * @return all the dashlets
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIDashlet> loadDashlets() throws ServerException;

	/**
	 * Saves the user's dashlet configuration
	 * 
	 * @param dashlets the dashlets to save
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void saveUserDashlets(List<GUIDashlet> dashlets) throws ServerException;

	/**
	 * Gets a dashlet
	 * 
	 * @param name the name of the dashlet
	 * 
	 * @return the dashlet retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIDashlet get(String name) throws ServerException;

	public void delete(long dashletId) throws ServerException;

	public static class Instance {
		private static DashletServiceAsync inst;

		private Instance() {
		}

		public static DashletServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(DashletService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}