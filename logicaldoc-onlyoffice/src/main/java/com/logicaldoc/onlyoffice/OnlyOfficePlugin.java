package com.logicaldoc.onlyoffice;

import com.logicaldoc.onlyoffice.controllers.OnlyOfficeIndex;
import com.logicaldoc.onlyoffice.controllers.OnlyOfficeEditor;
import com.logicaldoc.util.plugin.LogicalDOCPlugin;
import com.logicaldoc.util.plugin.PluginException;

/**
 * Entry-point for the OnlyOffice plug-in
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 9.1
 */
public class OnlyOfficePlugin extends LogicalDOCPlugin {

	@Override
	public void install() throws PluginException {
		super.install();

		addServlet("OnlyOfficeEditor", OnlyOfficeEditor.class.getName(), "/onlyoffice/editor");
		addServlet("OnlyOfficeIndex", OnlyOfficeIndex.class.getName(), "/onlyoffice/IndexServlet");

		setRestartRequired();
	}
}