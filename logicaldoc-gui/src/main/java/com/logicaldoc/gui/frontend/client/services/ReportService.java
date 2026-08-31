package com.logicaldoc.gui.frontend.client.services;

import java.util.List;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.logicaldoc.gui.common.client.LDRpcRequestBuilder;
import com.logicaldoc.gui.common.client.ServerException;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIReport;

/**
 * The client side stub for the Report Service. This service gives all needed
 * methods to handle reports.
 */
@RemoteServiceRelativePath("report")
public interface ReportService extends RemoteService {
	/**
	 * Deletes a given report
	 * 
	 * @param id identifier of the report to delete
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void delete(long id) throws ServerException;

	/**
	 * Updates a report
	 * 
	 * @param report the report to save
	 * 
	 * @return the saved report
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIReport save(GUIReport report) throws ServerException;

	/**
	 * Store the uploaded design file in the given report
	 * 
	 * @param id identifier of the report
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void storeUploadedDesign(long id) throws ServerException;

	/**
	 * Creates a new report
	 * 
	 * @param report the report to create
	 * 
	 * @return the just created report
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIReport create(GUIReport report) throws ServerException;

	/**
	 * Loads a given report from the database
	 * 
	 * @param id identifier of the records
	 * @param withLog flag to ask for the logs
	 * 
	 * @return the report retrieved by the server application
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public GUIReport getReport(long id, boolean withLog) throws ServerException;

	/**
	 * Loads all the reports
	 * 
	 * @return all the available reports
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIReport> getReports() throws ServerException;

	/**
	 * Loads the attributes defined in the given report
	 * 
	 * @param id identifiers of the report
	 * 
	 * @return the parameters of the report
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public List<GUIAttribute> getReportParameters(long id) throws ServerException;

	/**
	 * Changes a report enabled/disabled status
	 * 
	 * @param id identifier of the report
	 * @param enabled the new enabled status
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void changeStatus(long id, boolean enabled) throws ServerException;

	/**
	 * Processes a report
	 * 
	 * @param id identifier of the report
	 * @param parameters the values to be used in generating the report
	 * 
	 * @throws ServerException an error happened in the server application
	 */
	public void execute(long id, List<GUIAttribute> parameters) throws ServerException;

	public static class Instance {
		private static ReportServiceAsync inst;

		private Instance() {
		}

		public static ReportServiceAsync get() {
			if (inst == null) {
				inst = GWT.create(ReportService.class);
				((ServiceDefTarget) inst).setRpcRequestBuilder(new LDRpcRequestBuilder());
			}
			return inst;
		}
	}
}