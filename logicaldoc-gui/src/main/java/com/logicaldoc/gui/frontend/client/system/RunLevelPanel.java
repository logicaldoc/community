package com.logicaldoc.gui.frontend.client.system;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.data.AspectsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.HeaderSpan;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This panel handles the settings related to the Runlevel
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class RunLevelPanel extends VLayout {

	private static final String DEVEL = "devel";

	private static final String SLAVE = "slave";

	private static final String BULKLOAD = "bulkload";

	private static final String DEFAULT = "default";

	private static final String ASPECT = "aspect.";

	private static final String RUNLEVEL = "runlevel";

	private SelectItem currentRunlevel;

	private ListGrid aspects;

	public RunLevelPanel() {
		setWidth100();
		setHeight100();

		setMembersMargin(10);
	}

	@Override
	public void onDraw() {
		ToolStrip toolbar = new ToolStrip();
		toolbar.setWidth100();
		ToolStripButton save = new ToolStripButton(I18N.message("save"));
		save.setDisabled("demo".equals(Session.get().getConfig(RUNLEVEL)));
		save.addClickHandler(event -> onSave());
		currentRunlevel = ItemFactory.newRunlevelSelector();

		toolbar.addFormItem(currentRunlevel);
		toolbar.addSeparator();
		toolbar.addButton(save);

		Layout layout = new VLayout();
		layout.setWidth100();
		layout.setHeight100();
		layout.addMember(toolbar);
		layout.addMember(prepareAspectsTable());

		Tab tab = new Tab();
		tab.setTitle(I18N.message(RUNLEVEL));
		tab.setPane(layout);

		TabSet tabs = new TabSet();
		tabs.setTabs(tab);

		setMembers(tabs);
	}

	protected void onSave() {
		final List<GUIParameter> settings = new ArrayList<>();
		settings.add(new GUIParameter(RUNLEVEL, currentRunlevel.getValueAsString()));
		settings.add(new GUIParameter("runlevel.back", currentRunlevel.getValueAsString()));
		for (ListGridRecord rec : aspects.getRecords()) {
			settings.add(new GUIParameter(ASPECT + rec.getAttributeAsString("id") + ".default",
					rec.getAttributeAsString(DEFAULT)));
			settings.add(new GUIParameter(ASPECT + rec.getAttributeAsString("id") + ".bulkload",
					rec.getAttributeAsString(BULKLOAD)));
			settings.add(new GUIParameter(ASPECT + rec.getAttributeAsString("id") + ".slave",
					rec.getAttributeAsString(SLAVE)));
			settings.add(new GUIParameter(ASPECT + rec.getAttributeAsString("id") + ".devel",
					rec.getAttributeAsString(DEVEL)));
			settings.add(new GUIParameter(ASPECT + rec.getAttributeAsString("id") + ".demo",
					rec.getAttributeAsString("demo")));
		}

		SettingService.Instance.get().saveSettings(settings, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void arg) {
				GuiLog.info(I18N.message("settingssaved"));
				for (GUIParameter param : settings)
					Session.get().setConfig(param.getName(), param.getValue());
				SC.say(I18N.message("settingssaved") + "\n" + I18N.message("suggestedtorestart"));
			}
		});
	}

	private ListGrid prepareAspectsTable() {
		ListGridField id = new ListGridField("id", I18N.message("aspect"), 300);
		id.setCanEdit(false);
		id.setCanSort(false);
		id.setAutoFitWidth(true);
		id.setCellFormatter((value, rec, rowNum, colNum) -> I18N.message(ASPECT + rec.getAttributeAsString("id")));

		ListGridField defaultField = new ListGridField(DEFAULT, I18N.message(DEFAULT), 60);
		defaultField.setType(ListGridFieldType.BOOLEAN);
		defaultField.setCanEdit(true);
		defaultField.setAutoFitWidth(true);
		defaultField.setCanSort(false);

		ListGridField bulkload = new ListGridField(BULKLOAD, I18N.message(BULKLOAD), 60);
		bulkload.setType(ListGridFieldType.BOOLEAN);
		bulkload.setCanEdit(true);
		bulkload.setAutoFitWidth(true);
		bulkload.setCanSort(false);

		ListGridField slave = new ListGridField(SLAVE, I18N.message(SLAVE), 60);
		slave.setType(ListGridFieldType.BOOLEAN);
		slave.setCanEdit(true);
		slave.setAutoFitWidth(true);
		slave.setCanSort(false);

		ListGridField devel = new ListGridField(DEVEL, I18N.message(DEVEL), 60);
		devel.setType(ListGridFieldType.BOOLEAN);
		devel.setCanEdit(true);
		devel.setAutoFitWidth(true);
		devel.setCanSort(false);

		ListGridField demo = new ListGridField("demo", I18N.message("demo"), 60);
		demo.setType(ListGridFieldType.BOOLEAN);
		demo.setCanEdit(true);
		demo.setAutoFitWidth(true);
		demo.setCanSort(false);

		aspects = new ListGrid();
		aspects.setEmptyMessage(I18N.message("notitemstoshow"));
		aspects.setCanFreezeFields(true);
		aspects.setAutoFetchData(true);
		aspects.setDataSource(new AspectsDS());
		aspects.setFields(id, defaultField, bulkload, slave, devel, demo);
		aspects.setHeaderHeight(44);
		aspects.setHeaderSpans(
				new HeaderSpan(I18N.message("runlevels"), new String[] { DEFAULT, BULKLOAD, SLAVE, DEVEL, "demo" }));

		return aspects;
	}
}