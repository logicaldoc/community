package com.logicaldoc.gui.frontend.client.security;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUISequence;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.grid.DateListGridField;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.SelectionStyle;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.grid.CellFormatter;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.grid.events.CellContextClickEvent;
import com.smartgwt.client.widgets.grid.events.CellContextClickHandler;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;
import com.smartgwt.client.widgets.menu.events.MenuItemClickEvent;

/**
 * This panel shows the settings for the brute force attack countermeasures.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.2
 */
public class BruteForcePanel extends AdminPanel {

	private ValuesManager vm = new ValuesManager();

	private ListGrid blockedEntities;

	public BruteForcePanel() {
		super("bruteforceprevention");

		IButton save = new IButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new ClickHandler() {

			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		});

		addMember(save);

		SettingService.Instance.get().loadSettingsByNames(
				new String[] { "throttle.enabled", "throttle.username.max", "throttle.username.wait",
						"throttle.username.wait", "throttle.ip.max", "throttle.ip.wait" },
				new AsyncCallback<GUIParameter[]>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIParameter[] params) {
						Map<String, String> p = new HashMap<>();
						for (GUIParameter par : params)
							p.put(par.getName(), par.getValue());
						initForm(p);
					}
				});
	}

	private void initForm(Map<String, String> params) {
		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.LEFT);

		RadioGroupItem enabled = ItemFactory.newBooleanSelector("eenabled", "bruteforcepreventionenabled");
		enabled.setValue("true".equals(params.get("throttle.enabled")) ? "yes" : "no");
		enabled.setWrapTitle(false);
		enabled.setTitleOrientation(TitleOrientation.LEFT);

		SpinnerItem usernameMax = ItemFactory.newSpinnerItem("usernamemax", "maxsameusernamefailedattempts",
				(Integer) null);
		usernameMax.setMin(0);
		usernameMax.setWrapTitle(false);
		try {
			usernameMax.setValue(Integer.parseInt(params.get("throttle.username.max")));
		} catch (Throwable t) {
			// Nothing to do
		}

		SpinnerItem usernameWait = ItemFactory.newSpinnerItem("usernamewait", "sameusernamewait", (Integer) null);
		usernameWait.setMin(0);
		usernameWait.setHint(I18N.message("minutes"));
		usernameWait.setWrapTitle(false);
		try {
			usernameWait.setValue(Integer.parseInt(params.get("throttle.username.wait")));
		} catch (Throwable t) {
			// Nothing to do
		}

		SpinnerItem ipMax = ItemFactory.newSpinnerItem("ipmax", "maxsameipfailedattempts", (Integer) null);
		ipMax.setMin(0);
		ipMax.setWrapTitle(false);
		try {
			ipMax.setValue(Integer.parseInt(params.get("throttle.ip.max")));
		} catch (Throwable t) {
			// Nothing to do
		}

		SpinnerItem ipWait = ItemFactory.newSpinnerItem("ipwait", "sameipwait", (Integer) null);
		ipWait.setMin(0);
		ipWait.setWrapTitle(false);
		ipWait.setHint(I18N.message("minutes"));
		try {
			ipWait.setValue(Integer.parseInt(params.get("throttle.ip.wait")));
		} catch (Throwable t) {
			// Nothing to do
		}

		form.setItems(enabled, usernameMax, usernameWait, ipMax, ipWait);

		body.addMember(form);

		SecurityService.Instance.get().loadBlockedEntities(new AsyncCallback<GUISequence[]>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUISequence[] seqs) {
				prepareBlockedEntriesGrid(seqs);
			}
		});
	}

	private void prepareBlockedEntriesGrid(GUISequence[] data) {
		ListGridField id = new ListGridField("id", I18N.message("id"));
		id.setWidth(60);
		id.setHidden(true);

		ListGridField entity = new ListGridField("entity", I18N.message("blockedusernameip"));
		entity.setWidth(200);
		entity.setCellFormatter(new CellFormatter() {

			@Override
			public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
				String name = ((String) value).substring(((String) value).lastIndexOf('-') + 1);
				return name;
			}
		});

		ListGridField attempts = new ListGridField("attempts", I18N.message("attempts"));
		attempts.setWidth(80);
		attempts.setAlign(Alignment.CENTER);

		ListGridField lastAttempt = new DateListGridField("lastmodified", "lastattempt");

		blockedEntities = new ListGrid();
		blockedEntities.setShowAllRecords(true);
		blockedEntities.setCanEdit(false);
		blockedEntities.setWidth100();
		blockedEntities.setHeight100();
		blockedEntities.setSelectionType(SelectionStyle.MULTIPLE);

		List<ListGridRecord> records = new ArrayList<>();
		if (data != null)
			for (GUISequence cid : data) {
				ListGridRecord rec = new ListGridRecord();
				rec.setAttribute("id", cid.getId());
				rec.setAttribute("entity", cid.getName());
				rec.setAttribute("attempts", cid.getValue());
				rec.setAttribute("lastmodified", cid.getLastModified());
				records.add(rec);
			}
		blockedEntities.setData(records.toArray(new ListGridRecord[0]));

		blockedEntities.setFields(id, entity, attempts, lastAttempt);

		blockedEntities.addCellContextClickHandler(new CellContextClickHandler() {
			@Override
			public void onCellContextClick(CellContextClickEvent event) {
				showContextMenu();
				event.cancel();
			}
		});

		body.addMember(blockedEntities);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] records = blockedEntities.getSelectedRecords();
		final long[] ids = new long[records.length];
		for (int i = 0; i < records.length; i++)
			ids[i] = records[i].getAttributeAsLong("id");

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(new com.smartgwt.client.widgets.menu.events.ClickHandler() {
			public void onClick(MenuItemClickEvent event) {
				LD.ask(I18N.message("question"), I18N.message("confirmdelete"),(Boolean value) -> {
					if (Boolean.TRUE.equals(value)) {
							SecurityService.Instance.get().removeBlockedEntities(ids, new AsyncCallback<Void>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(Void result) {
									blockedEntities.removeSelectedData();
								}
							});
						}
				});
			}
		});

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	public void onSave() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		@SuppressWarnings("unchecked")
		Map<String, Object> values = (Map<String, Object>) vm.getValues();
		GUIParameter[] params = new GUIParameter[5];
		params[0] = new GUIParameter("throttle.enabled",
				"yes".equals(values.get("eenabled").toString()) ? "true" : "false");
		params[1] = new GUIParameter("throttle.username.max", values.get("usernamemax").toString());
		params[2] = new GUIParameter("throttle.username.wait", values.get("usernamewait").toString());
		params[3] = new GUIParameter("throttle.ip.max", values.get("ipmax").toString());
		params[4] = new GUIParameter("throttle.ip.wait", values.get("ipwait").toString());

		SettingService.Instance.get().saveSettings(params, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg) {
				GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}
}