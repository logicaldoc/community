package com.logicaldoc.gui.frontend.client.system.task;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIValue;
import com.logicaldoc.gui.common.client.data.JobsDS;
import com.logicaldoc.gui.common.client.grid.DateListGridField;
import com.logicaldoc.gui.common.client.grid.RefreshableListGrid;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.SystemService;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * Displays a list of scheduled jobs.
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.7.4
 */
public class JobsPanel extends VLayout {

	private static final String TRIGGER = "trigger";

	private RefreshableListGrid list;

	private SpinnerItem max;

	private SelectItem group;

	public JobsPanel() {
		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		max = ItemFactory.newSpinnerItem("max", "display", 100, 10, null);
		max.setWidth(70);
		max.setStep(20);
		max.setSaveOnEnter(true);
		max.setImplicitSave(true);
		max.setHint(I18N.message("elements"));
		toolStrip.addFormItem(max);

		group = ItemFactory.newJobGroupSelector();
		toolStrip.addFormItem(group);

		toolStrip.addSeparator();
		ToolStripButton refresh = new ToolStripButton(I18N.message("refresh"));
		toolStrip.addButton(refresh);
		toolStrip.addFill();

		addMember(toolStrip);

		refresh.addClickHandler(event -> refresh());

		group.addChangedHandler(event -> refresh());

		max.addChangedHandler(event -> refresh());
	}

	@Override
	public void onDraw() {
		ListGridField job = new ListGridField("name", I18N.message("job"), 250);

		ListGridField trigger = new ListGridField(TRIGGER, I18N.message(TRIGGER), 300);

		ListGridField tenantId = new ListGridField("tenantId", I18N.message("tenantId"), 80);
		tenantId.setHidden(true);

		ListGridField tenant = new ListGridField("tenant", I18N.message("tenant"), 110);

		ListGridField description = new ListGridField("description", I18N.message("description"), 300);
		description.setWidth("*");

		ListGridField previousFire = new DateListGridField("previousFire", "laststart");

		ListGridField nextFire = new DateListGridField("nextFire", "nextstart");

		list = new RefreshableListGrid();
		list.setEmptyMessage(I18N.message("notitemstoshow"));

		list.setShowRecordComponents(true);
		list.setShowRecordComponentsByCell(true);
		list.setCanFreezeFields(true);
		list.setAutoFetchData(true);
		list.setSelectionType(SelectionStyle.MULTIPLE);
		list.setFields(job, trigger, previousFire, nextFire, tenant, tenantId, description);

		list.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		addMember(list);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();
		MenuItem unschedule = new MenuItem();
		unschedule.setTitle(I18N.message("unschedule"));
		unschedule
				.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmunschedule"), value -> {
					if (Boolean.TRUE.equals(value)) {
						LD.contactingServer();

						List<GUIValue> selectedJobs = new ArrayList<>();
						ListGridRecord[] selection = list.getSelectedRecords();
						for (ListGridRecord rec : selection)
							selectedJobs.add(
									new GUIValue(rec.getAttributeAsString(TRIGGER), rec.getAttributeAsString("group")));

						SystemService.Instance.get().unscheduleJobs(selectedJobs, new DefaultAsyncCallback<>() {
							@Override
							public void onSuccess(Void result) {
								LD.clearPrompt();
								refresh();
							}
						});
					}
				}));

		contextMenu.setItems(unschedule);
		contextMenu.showContextMenu();
	}

	private void refresh() {
		if (group.getValue() != null)
			list.refresh(new JobsDS(max.getValueAsInteger(), group.getValueAsString()));
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}