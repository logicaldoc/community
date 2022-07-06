package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This panel shows the Parameters settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class ParametersPanel extends AdminPanel {

	public ParametersPanel() {
		super("parameters");

		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	protected void onDraw() {
		SettingService.Instance.get().loadSettings(new AsyncCallback<GUIParameter[]>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIParameter[] settings) {
				initGUI(settings);
			}

		});
	}

	private void initGUI(GUIParameter[] settings) {
		ListGridField parameter = new ListGridField("parameter", I18N.message("parameter"));
		parameter.setAutoFitWidth(true);
		parameter.setCanEdit(false);

		ListGridField value = new ListGridField("value", I18N.message("value"));
		value.setWidth("*");
		value.setCanEdit(true);

		ListGrid parametersGrid = new ListGrid();
		parametersGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		parametersGrid.setWidth100();
		parametersGrid.setHeight100();
		parametersGrid.setAutoFetchData(true);
		parametersGrid.setCanSelectAll(false);
		parametersGrid.setSelectionType(SelectionStyle.SINGLE);
		parametersGrid
				.setCanEdit(!Session.get().isDemo() && Session.get().isAdmin() && Session.get().isDefaultTenant());
		parametersGrid.setAutoConfirmSaveEdits(true);
		parametersGrid.setEditByCell(true);
		parametersGrid.setCanReorderRecords(true);
		parametersGrid.setShowRecordComponents(true);
		parametersGrid.setShowRecordComponentsByCell(true);
		parametersGrid.setFields(parameter, value);

		ListGridRecord[] records = new ListGridRecord[settings.length];
		int i = 0;
		for (GUIParameter guiParameter : settings) {
			records[i] = new ListGridRecord();
			records[i].setAttribute("value", guiParameter.getValue());
			records[i].setAttribute("parameter", guiParameter.getName());
			i++;
		}
		parametersGrid.setRecords(records);

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				ArrayList<GUIParameter> params = new ArrayList<GUIParameter>();
				ListGridRecord[] records = parametersGrid.getRecords();
				for (ListGridRecord record : records) {
					GUIParameter param = new GUIParameter(record.getAttributeAsString("parameter"),
							record.getAttributeAsString("value"));
					Session.get().getInfo().setConfig(param.getName(), param.getValue());
					params.add(param);
				}

				SettingService.Instance.get().saveSettings(params.toArray(new GUIParameter[0]),
						new AsyncCallback<Void>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void ret) {
								GuiLog.info(I18N.message("settingssaved"), null);
							}
						});
			}
		});
		save.setDisabled(Session.get().isDemo() || !Session.get().isAdmin() || !Session.get().isDefaultTenant());

		body.setMembers(parametersGrid);
		addMember(save);
	}
}
