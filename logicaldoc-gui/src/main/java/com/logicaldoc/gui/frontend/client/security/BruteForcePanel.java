package com.logicaldoc.gui.frontend.client.security;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUISequence;
import com.logicaldoc.gui.common.client.data.UsersDS;
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
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridField;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.menu.MenuItem;

/**
 * This panel shows the settings for the brute force attack countermeasures.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.2
 */
public class BruteForcePanel extends AdminPanel {

	private static final String MINUTES = "minutes";

	private static final String RECIPIENTS = "recipients";

	private static final String ATTEMPTS = "attempts";

	private static final String THROTTLE_IP_WAIT = "throttle.ip.wait";

	private static final String THROTTLE_IP_MAX = "throttle.ip.max";

	private static final String THROTTLE_APIKEY_WAIT = "throttle.apikey.wait";

	private static final String THROTTLE_APIKEY_MAX = "throttle.apikey.max";

	private static final String THROTTLE_ALERT_RECIPIENTS = "throttle.alert.recipients";

	private static final String THROTTLE_USERNAME_WAIT = "throttle.username.wait";

	private static final String THROTTLE_USERNAME_MAX = "throttle.username.max";

	private static final String THROTTLE_USERNAME_DISABLEUSER = "throttle.username.disableuser";

	private static final String THROTTLE_ENABLED = "throttle.enabled";

	private ValuesManager vm = new ValuesManager();

	private ListGrid blockedEntities;

	public BruteForcePanel() {
		super("bruteforceprevention");

		IButton save = new IButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(event -> onSave());

		addMember(save);

		SettingService.Instance.get()
				.loadSettingsByNames(
						Arrays.asList(THROTTLE_ENABLED, THROTTLE_USERNAME_MAX, THROTTLE_USERNAME_WAIT,
								THROTTLE_USERNAME_DISABLEUSER, THROTTLE_USERNAME_WAIT, THROTTLE_IP_MAX,
								THROTTLE_IP_WAIT, THROTTLE_APIKEY_MAX, THROTTLE_APIKEY_WAIT, THROTTLE_ALERT_RECIPIENTS),
						new AsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(List<GUIParameter> params) {
								Map<String, String> p = new HashMap<>();
								for (GUIParameter par : params)
									p.put(par.getName(), par.getValue());
								initForm(p);
							}
						});
	}

	private int intValue(String str) {
		try {
			return Integer.parseInt(str);
		} catch (Exception t) {
			return 0;
		}
	}

	private void initForm(Map<String, String> params) {
		DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.LEFT);

		ToggleItem enabled = ItemFactory.newToggleItem("eenabled", "bruteforcepreventionenabled",
				Boolean.valueOf(params.get(THROTTLE_ENABLED)));
		enabled.setWrapTitle(false);
		enabled.setTitleOrientation(TitleOrientation.LEFT);

		SpinnerItem usernameMax = ItemFactory.newSpinnerItem("usernamemax", "maxsameusernamefailedattempts",
				intValue(params.get(THROTTLE_USERNAME_MAX)));
		usernameMax.setMin(0);
		usernameMax.setWrapTitle(false);

		SpinnerItem usernameWait = ItemFactory.newSpinnerItem("usernamewait", "sameusernamewait",
				intValue(params.get(THROTTLE_USERNAME_WAIT)));
		usernameWait.setMin(0);
		usernameWait.setHint(I18N.message(MINUTES));
		usernameWait.setWrapTitle(false);

		usernameWait.setDisabled(Boolean.valueOf(params.get(THROTTLE_USERNAME_DISABLEUSER)));

		ToggleItem usernameDisableUser = ItemFactory.newToggleItem("usernamedisableuser",
				"disableuserafterfailedusername", Boolean.valueOf(params.get(THROTTLE_USERNAME_DISABLEUSER)));
		usernameDisableUser.setWrapTitle(false);
		usernameDisableUser.setTitleOrientation(TitleOrientation.LEFT);
		usernameDisableUser.addChangedHandler(event -> usernameWait.setDisabled("yes".equals(event.getValue())));

		SpinnerItem ipMax = ItemFactory.newSpinnerItem("ipmax", "maxsameipfailedattempts",
				intValue(params.get(THROTTLE_IP_MAX)));
		ipMax.setMin(0);
		ipMax.setWrapTitle(false);

		SpinnerItem ipWait = ItemFactory.newSpinnerItem("ipwait", "sameipwait", intValue(params.get(THROTTLE_IP_WAIT)));
		ipWait.setMin(0);
		ipWait.setWrapTitle(false);
		ipWait.setHint(I18N.message(MINUTES));

		SpinnerItem apikeyMax = ItemFactory.newSpinnerItem("apikeymax", "maxsameapikeyfailedattempts",
				intValue(params.get(THROTTLE_APIKEY_MAX)));
		apikeyMax.setMin(0);
		apikeyMax.setWrapTitle(false);

		SpinnerItem apikeyWait = ItemFactory.newSpinnerItem("apikeywait", "sameapikeywait",
				intValue(params.get(THROTTLE_APIKEY_WAIT)));
		apikeyWait.setMin(0);
		apikeyWait.setWrapTitle(false);
		apikeyWait.setHint(I18N.message(MINUTES));

		MultiComboBoxItem recipients = ItemFactory.newMultiComboBoxItem(RECIPIENTS, "alertrecipients",
				new UsersDS(null, false, false),
				params.get(THROTTLE_ALERT_RECIPIENTS) != null && !params.get(THROTTLE_ALERT_RECIPIENTS).trim().isEmpty()
						? params.get(THROTTLE_ALERT_RECIPIENTS).trim().split(",")
						: null);
		recipients.setValueField("username");
		recipients.setDisplayField("username");

		form.setItems(enabled, usernameMax, usernameDisableUser, usernameWait, ipMax, ipWait, apikeyMax, apikeyWait,
				recipients);

		body.addMember(form);

		SecurityService.Instance.get().loadBlockedEntities(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(List<GUISequence> seqs) {
				prepareBlockedEntriesGrid(seqs);
			}
		});
	}

	private void prepareBlockedEntriesGrid(List<GUISequence> data) {
		ListGridField id = new ListGridField("id", I18N.message("id"));
		id.setWidth(60);
		id.setHidden(true);

		ListGridField entity = new ListGridField("entity", I18N.message("blockedusernameip"));
		entity.setWidth(200);
		entity.setCellFormatter(
				(value, rec, rowNum, colNum) -> ((String) value).substring(((String) value).lastIndexOf('-') + 1));

		ListGridField attempts = new ListGridField(ATTEMPTS, I18N.message(ATTEMPTS));
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
		for (GUISequence cid : data) {
			ListGridRecord rec = new ListGridRecord();
			rec.setAttribute("id", cid.getId());
			rec.setAttribute("entity", cid.getName());
			rec.setAttribute(ATTEMPTS, cid.getValue());
			rec.setAttribute("lastmodified", cid.getLastModified());
			records.add(rec);
		}
		blockedEntities.setData(records.toArray(new ListGridRecord[0]));

		blockedEntities.setFields(id, entity, attempts, lastAttempt);

		blockedEntities.addCellContextClickHandler(event -> {
			showContextMenu();
			event.cancel();
		});

		body.addMember(blockedEntities);
	}

	private void showContextMenu() {
		Menu contextMenu = new Menu();

		final ListGridRecord[] records = blockedEntities.getSelectedRecords();
		final List<Long> ids = new ArrayList<>();
		for (int i = 0; i < records.length; i++)
			ids.add(records[i].getAttributeAsLong("id"));

		MenuItem delete = new MenuItem();
		delete.setTitle(I18N.message("ddelete"));
		delete.addClickHandler(event -> LD.ask(I18N.message("question"), I18N.message("confirmdelete"), confirm -> {
			if (Boolean.TRUE.equals(confirm)) {
				SecurityService.Instance.get().removeBlockedEntities(ids, new AsyncCallback<>() {
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
		}));

		contextMenu.setItems(delete);
		contextMenu.showContextMenu();
	}

	public void onSave() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		List<GUIParameter> params = new ArrayList<>();
		params.add(new GUIParameter(THROTTLE_ENABLED, vm.getValueAsString("eenabled")));
		params.add(new GUIParameter(THROTTLE_USERNAME_MAX, vm.getValueAsString("usernamemax")));
		params.add(new GUIParameter(THROTTLE_USERNAME_WAIT, vm.getValueAsString("usernamewait")));
		params.add(new GUIParameter(THROTTLE_IP_MAX, vm.getValueAsString("ipmax")));
		params.add(new GUIParameter(THROTTLE_IP_WAIT, vm.getValueAsString("ipwait")));
		params.add(new GUIParameter(THROTTLE_APIKEY_MAX, vm.getValueAsString("apikeymax")));
		params.add(new GUIParameter(THROTTLE_APIKEY_WAIT, vm.getValueAsString("apikeywait")));
		params.add(new GUIParameter(THROTTLE_USERNAME_DISABLEUSER, vm.getValueAsString("usernamedisableuser")));

		if (vm.getValueAsString(RECIPIENTS) != null) {
			@SuppressWarnings("unchecked")
			ArrayList<String> usernames = (ArrayList<String>) vm.getValue(RECIPIENTS);
			params.add(
					new GUIParameter(THROTTLE_ALERT_RECIPIENTS, usernames.stream().collect(Collectors.joining(","))));
		} else {
			params.add(new GUIParameter(THROTTLE_ALERT_RECIPIENTS, ""));
		}

		SettingService.Instance.get().saveSettings(params, new AsyncCallback<>() {

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