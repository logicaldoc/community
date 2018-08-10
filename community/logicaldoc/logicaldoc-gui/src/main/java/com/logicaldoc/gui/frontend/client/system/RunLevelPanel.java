package com.logicaldoc.gui.frontend.client.system;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.data.AspectsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.ListGridFieldType;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.HeaderSpan;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.SectionStack;
import com.smartgwt.client.widgets.layout.SectionStackSection;
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
		save.setDisabled("demo".equals(Session.get().getConfig("runlevel")));
		save.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});
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
		tab.setTitle(I18N.message("runlevel"));
		tab.setPane(layout);

		TabSet tabs = new TabSet();
		tabs.setTabs(tab);

		setMembers(tabs);
	}

	protected void onSave() {
		final List<GUIParameter> settings = new ArrayList<GUIParameter>();
		settings.add(new GUIParameter("runlevel", currentRunlevel.getValueAsString()));
		for (ListGridRecord rec : aspects.getRecords()) {
			settings.add(new GUIParameter("aspect." + rec.getAttributeAsString("id") + ".default", rec
					.getAttributeAsString("default")));
			settings.add(new GUIParameter("aspect." + rec.getAttributeAsString("id") + ".bulkload", rec
					.getAttributeAsString("bulkload")));
			settings.add(new GUIParameter("aspect." + rec.getAttributeAsString("id") + ".devel", rec
					.getAttributeAsString("devel")));
			settings.add(new GUIParameter("aspect." + rec.getAttributeAsString("id") + ".demo", rec
					.getAttributeAsString("demo")));
		}

		SettingService.Instance.get().saveSettings(settings.toArray(new GUIParameter[0]), new AsyncCallback<Void>() {
			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg) {
				Log.info(I18N.message("settingssaved"));
				for (GUIParameter param : settings)
					Session.get().setConfig(param.getName(), param.getValue());
				SC.say(I18N.message("settingssaved") + "\n" + I18N.message("suggestedtorestart"));
			}
		});
	}

	private SectionStack prepareAspectsTable() {
		ListGridField id = new ListGridField("id", I18N.message("aspect"), 300);
		id.setCanEdit(false);
		id.setAutoFitWidth(true);
		id.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord record, int rowNum, int colNum) {
				return I18N.message("aspect." + record.getAttributeAsString("id"));
			}
		});

		ListGridField _default = new ListGridField("default", I18N.message("default"), 60);
		_default.setType(ListGridFieldType.BOOLEAN);
		_default.setCanEdit(true);
		_default.setAutoFitWidth(true);

		ListGridField bulkload = new ListGridField("bulkload", I18N.message("bulkload"), 60);
		bulkload.setType(ListGridFieldType.BOOLEAN);
		bulkload.setCanEdit(true);
		bulkload.setAutoFitWidth(true);

		ListGridField devel = new ListGridField("devel", I18N.message("devel"), 60);
		devel.setType(ListGridFieldType.BOOLEAN);
		devel.setCanEdit(true);
		devel.setAutoFitWidth(true);

		ListGridField demo = new ListGridField("demo", I18N.message("demo"), 60);
		demo.setType(ListGridFieldType.BOOLEAN);
		demo.setCanEdit(true);
		demo.setAutoFitWidth(true);

		aspects = new ListGrid();
		aspects.setEmptyMessage(I18N.message("notitemstoshow"));
		aspects.setCanFreezeFields(true);
		aspects.setAutoFetchData(true);
		aspects.setDataSource(new AspectsDS());
		aspects.setFields(id, _default, bulkload, devel, demo);
		aspects.setHeaderHeight(44);
		aspects.setHeaderSpans(new HeaderSpan(I18N.message("runlevels"), new String[] { "default", "bulkload", "devel",
				"demo" }));

		SectionStack stack = new SectionStack();
		stack.setWidth100();
		stack.setHeight100();

		SectionStackSection section = new SectionStackSection(I18N.message("aspects"));
		section.setCanCollapse(false);
		section.setExpanded(true);

		section.setItems(aspects);
		stack.setSections(section);
		return stack;
	}
}