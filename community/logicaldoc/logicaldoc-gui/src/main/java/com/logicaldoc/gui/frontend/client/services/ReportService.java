package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;
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
	 */
	public void delete(long id) throws ServerException;

	/**
	 * Updates a report
	 */
	public GUIReport save(GUIReport report) throws ServerException;

	/**
	 * Store the uploaded design file in the given report
	 */
	public void storeUploadedDesign(long id) throws ServerException;

	/**
	 * Creates a new report
	 */
	public GUIReport create(GUIReport report) throws ServerException;

	/**
	 * Loads a given report from the database
	 */
	public GUIReport getReport(long id, boolean withLog) throws ServerException;

	/**
	 * Loads all the reports
	 */
	public GUIReport[] getReports() throws ServerException;

	/**
	 * Loads the attributes defined in the given report
	 */
	public GUIAttribute[] getReportParameters(long id) throws ServerException;

	/**
	 * Changes a report enabled/disabled status
	 */
	public void changeStatus(long id, boolean enabled) throws ServerException;

	/**
	 * Processes a report
	 */
	public void execute(long id, GUIAttribute[] parameters) throws ServerException;

	public static class Instance {
		private static ReportServiceAsync instance;

		public static ReportServiceAsync get() {
			if (instance == null) {
				instance = GWT.create(ReportService.class);
			}
			return instance;
		}
	}
}