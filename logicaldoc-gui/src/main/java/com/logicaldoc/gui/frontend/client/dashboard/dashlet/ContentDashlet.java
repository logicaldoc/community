package com.logicaldoc.gui.frontend.client.dashboard.dashlet;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.DashletService;
import com.smartgwt.client.types.Overflow;
import com.smartgwt.client.widgets.HTMLFlow;

/**
 * Dashlet specialized in showing an HTML content.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.2.3
 */
public class ContentDashlet extends Dashlet {
	private HTMLFlow content = null;

	public ContentDashlet(GUIDashlet guiDashlet) {
		super(guiDashlet);
		setMembersMargin(0);
		initGUI();
	}

	private void initGUI() {
		if (content != null)
			removeChild(content);

		content = new HTMLFlow();
		content.setWidth100();
		content.setHeight100();
		content.setOverflow(Overflow.SCROLL);
		content.setContentsURL(Util.contextPath() + getDataSourceUrl());

		addMember(content);
	}
	
	@Override
	protected void refresh() {
		DashletService.Instance.get().get(guiDashlet.getId(), new AsyncCallback<GUIDashlet>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIDashlet dashlet) {
				guiDashlet = dashlet;
				initGUI();
			}
		});
	}
}