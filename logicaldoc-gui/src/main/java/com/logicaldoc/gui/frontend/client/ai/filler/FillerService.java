package com.logicaldoc.gui.frontend.client.ai.filler;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the {@link FillerService}. This service gives all
 * needed methods to handle fillers.
 * 
 * @author Matteo Desiato - LogicalDOC
 * @since 9.2.3
 */
@RemoteServiceRelativePath("filler")
public interface FillerService extends RemoteService {

	/**
	 * Deletes some fillers
	 * 
	 * @param fillerId identifiers of the fillers
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(List<Long> fillerId) throws ServerException;

	/**
	 * Creates or updates a filler
	 * 
	 * @param filler the filler to save
	 * 
	 * @return the saved filler
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIFiller save(GUIFiller filler) throws ServerException;

	/**
	 * Retrieves a filler from the data layer
	 * 
	 * @param fillerId identifier of the filler
	 * 
	 * @return the filler
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIFiller get(long fillerId) throws ServerException;

	public static class Instance {
		private static FillerServiceAsync inst;

		private Instance() {
		}

		public static FillerServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(FillerService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}