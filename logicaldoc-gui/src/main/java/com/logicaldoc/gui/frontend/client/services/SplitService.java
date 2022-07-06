package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;

/**
 * The client side stub for the Split Service.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4
 */
@RemoteServiceRelativePath("split")
public interface SplitService extends RemoteService {
	public final static int POLICY_ALLPAGES = 0;

	public final static int POLICY_SELECTION = 1;

	public final static int POLICY_BLANKPAGE = 2;

	public final static int POLICY_BARCODE = 3;

	public final static int POLICY_TEXT = 4;

	public final static int SEPARATOR_SKIP = 0;

	public final static int SEPARATOR_CURRENTSEG = 1;

	public final static int SEPARATOR_NEXTSEG = 2;

	/**
	 * Splits a document using different options, the splitted segments are
	 * saved in the same folder as the original PDF with -N prefix.
	 * 
	 * @param docId the document to split
	 * @param policy of split (see the TYPE_ constants)
	 * @param separator separator way to handle the separator (see the TYPE_
	 *        constants)
	 * @param expression the expression to use processing the split
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void split(long docId, int policy, int separator, String expression) throws ServerException;

	public static class Instance {

		private static SplitServiceAsync instance;

		public static SplitServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(SplitService.class);
				((ServiceDefTarget) instance).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return instance;
		}
	}
}