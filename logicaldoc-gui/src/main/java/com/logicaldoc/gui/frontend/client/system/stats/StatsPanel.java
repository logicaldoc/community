package com.logicaldoc.gui.frontend.client.system.stats;

import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * General administration panel with statistics
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class StatsPanel extends VLayout {

	public StatsPanel() {
		setWidth100();
		setMembersMargin(10);
	}

	@Override
	public void onDraw() {
		SystemService.Instance.get().getStatistics(I18N.getLocale(), new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<List<GUIParameter>> parameters) {
				Label lastUpdateLabel = new Label("<b>" + I18N.message("lastupdate") + ": "
						+ parameters.get(4).get(0).getValue() + "</b>");
				lastUpdateLabel.setHeight(30);
				lastUpdateLabel.setAlign(Alignment.RIGHT);

				StatsPie charts = new StatsPie(parameters);

				setMembers(lastUpdateLabel, charts);
			}
		});
	}
}