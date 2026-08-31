package com.logicaldoc.gui.frontend.client.settings;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.IButton;
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

	private static final String VALUE = "value";

	private static final String PARAMETER = "parameter";

	public ParametersPanel() {
		super("parameters");

		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	protected void onDraw() {
		SettingService.Instance.get().loadSettings(new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(List<GUIParameter> settings) {
				initGUI(settings);
			}

		});
	}

	private void initGUI(List<GUIParameter> settings) {
		ListGridField parameter = new ListGridField(PARAMETER, I18N.message(PARAMETER));
		parameter.setAutoFitWidth(true);
		parameter.setCanEdit(false);

		ListGridField value = new ListGridField(VALUE, I18N.message(VALUE));
		value.setWidth("*");
		value.setCanEdit(true);

		ListGrid parametersGrid = new ListGrid();
		parametersGrid.setEmptyMessage(I18N.message("notitemstoshow"));
		parametersGrid.setWidth100();
		parametersGrid.setHeight100();
		parametersGrid.setAutoFetchData(true);
		parametersGrid.setCanSelectAll(false);
		parametersGrid.setCanSort(false);
		parametersGrid.setSelectionType(SelectionStyle.SINGLE);
		parametersGrid
				.setCanEdit(!Session.get().isDemo() && Session.get().isAdmin() && Session.get().isDefaultTenant());
		parametersGrid.setAutoConfirmSaveEdits(true);
		parametersGrid.setEditByCell(true);
		parametersGrid.setCanReorderRecords(true);
		parametersGrid.setShowRecordComponents(true);
		parametersGrid.setShowRecordComponentsByCell(true);
		parametersGrid.setFields(parameter, value);

		List<ListGridRecord> records = new ArrayList<>();
		for (GUIParameter guiParameter : settings) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute(VALUE, guiParameter.getValue());
			rec.setAttribute(PARAMETER, guiParameter.getName());
			records.add(rec);
		}
		parametersGrid.setRecords(records.toArray(new ListGridRecord[0]));

		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(event -> {
			ArrayList<GUIParameter> params = new ArrayList<>();
			for (ListGridRecord rec : parametersGrid.getRecords()) {
				GUIParameter param = new GUIParameter(rec.getAttributeAsString(PARAMETER),
						rec.getAttributeAsString(VALUE));
				Session.get().getInfo().setConfig(param.getName(), param.getValue());
				params.add(param);
			}

			SettingService.Instance.get().saveSettings(params, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(Void ret) {
					GuiLog.info(I18N.message("settingssaved"), null);
				}
			});
		});
		save.setDisabled(Session.get().isDemo() || !Session.get().isAdmin() || !Session.get().isDefaultTenant());

		body.setMembers(parametersGrid);
		addMember(save);
	}
}
