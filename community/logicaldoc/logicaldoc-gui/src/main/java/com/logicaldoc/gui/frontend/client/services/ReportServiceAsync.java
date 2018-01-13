package com.logicaldoc.gui.frontend.client.services;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIReport;

public interface ReportServiceAsync {

	void changeStatus(long id, boolean enabled, AsyncCallback<Void> callback);

	void delete(long id, AsyncCallback<Void> callback);

	void getReport(long id, boolean withLog, AsyncCallback<GUIReport> callback);

	void execute(long id, GUIAttribute[] paremeters, AsyncCallback<Void> callback);

	void save(GUIReport report, AsyncCallback<GUIReport> callback);

	void create(GUIReport report, AsyncCallback<GUIReport> callback);

	void getReports(AsyncCallback<GUIReport[]> callback);

	void getReportParameters(long id, AsyncCallback<GUIAttribute[]> callback);

	void storeUploadedDesign(long id, AsyncCallback<Void> callback);
}