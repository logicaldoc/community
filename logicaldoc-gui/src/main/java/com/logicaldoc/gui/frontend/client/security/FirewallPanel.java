package com.logicaldoc.gui.frontend.client.security;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel shows the firewall settings.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class FirewallPanel extends VLayout {

	private static final String WHITELIST = "whitelist";

	private static final String BLACKLIST = "blacklist";

	private static final String FIREWALL_ALLOW_URL_ENCODED_PERIOD = "firewall.allowUrlEncodedPeriod";

	private static final String FIREWALL_ALLOW_URL_ENCODED_SLASH = "firewall.allowUrlEncodedSlash";

	private static final String FIREWALL_ALLOW_URL_ENCODED_PERCENT = "firewall.allowUrlEncodedPercent";

	private static final String FIREWALL_ALLOW_BACK_SLASH = "firewall.allowBackSlash";

	private static final String FIREWALL_ALLOW_SEMICOLON = "firewall.allowSemicolon";

	private static final String FIREWALL_BLACKLIST = "firewall.blacklist";

	private static final String FIREWALL_WHITELIST = "firewall.whitelist";

	private static final String FIREWALL_ENABLED = "firewall.enabled";

	private ValuesManager vm = new ValuesManager();

	private GUIUser user;

	private ChangedHandler changedHandler;

	private DynamicForm form;

	public FirewallPanel(GUIUser user, ChangedHandler changedHandler) {
		super();
		this.user = user;
		this.changedHandler = changedHandler;

		setWidth100();
		setHeight100();
		setMembersMargin(20);

		initGUI();
		setMembers(form);
	}

	public FirewallPanel() {
		setWidth100();
		setMembersMargin(5);
		setMargin(5);

		initGUI();

		Tab tab = new Tab();
		tab.setTitle(I18N.message("firewall"));
		tab.setPane(form);

		TabSet tabs = new TabSet();
		tabs.setWidth100();
		tabs.setHeight100();
		tabs.setTabs(tab);

		IButton save = new IButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(event -> onSave());

		setMembers(tabs, save);
	}

	private void initGUI() {
		prepareForm();

		ToggleItem enabled = prepareEnabledSwitch();

		final TextAreaItem whitelist = prepareWhiteListItem();

		final TextAreaItem blacklist = prepareBlackListItem();

		ToggleItem allowSemicolon = prepareAllowSemicolonSwitch();

		ToggleItem allowBackSlash = prepareAllowBackSlashSwitch();

		ToggleItem allowUrlEncodedPercent = prepareAllowUrlEncodedPercentSwitch();

		ToggleItem allowUrlEncodedSlash = prepareAllowUrlEncodedSlashSwitch();

		ToggleItem allowUrlEncodedPeriod = prepareAllowUrlEncodedPeriodSwitch();

		if (user == null) {
			/*
			 * We are operating on application-wide filters
			 */
			initSettings(enabled, whitelist, blacklist, allowSemicolon, allowBackSlash, allowUrlEncodedPercent,
					allowUrlEncodedSlash, allowUrlEncodedPeriod);
		} else {
			/*
			 * We are operating on user's specific filters
			 */
			form.setItems(whitelist, blacklist);
			whitelist.setValue(user.getIpWhitelist() != null ? user.getIpWhitelist().replace(',', '\n') : "");
			blacklist.setValue(user.getIpBlacklist() != null ? user.getIpBlacklist().replace(',', '\n') : "");
		}
	}

	private void initSettings(ToggleItem enabled, final TextAreaItem whitelist, final TextAreaItem blacklist,
			ToggleItem allowSemicolon, ToggleItem allowBackSlash, ToggleItem allowUrlEncodedPercent,
			ToggleItem allowUrlEncodedSlash, ToggleItem allowUrlEncodedPeriod) {
		form.setItems(enabled, whitelist, blacklist, allowSemicolon, allowBackSlash, allowUrlEncodedPercent,
				allowUrlEncodedSlash, allowUrlEncodedPeriod);
		SettingService.Instance.get()
				.loadSettingsByNames(
						Arrays.asList(FIREWALL_ENABLED, FIREWALL_WHITELIST, FIREWALL_BLACKLIST,
								FIREWALL_ALLOW_SEMICOLON, FIREWALL_ALLOW_BACK_SLASH, FIREWALL_ALLOW_URL_ENCODED_PERCENT,
								FIREWALL_ALLOW_URL_ENCODED_SLASH, FIREWALL_ALLOW_URL_ENCODED_PERIOD),
						new AsyncCallback<>() {
							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(List<GUIParameter> params) {
								enabled.setValue(params.get(0).getValueAsBoolean());
								whitelist.setValue(params.get(1).getValue().replace(',', '\n'));
								blacklist.setValue(params.get(2).getValue().replace(',', '\n'));
								allowSemicolon.setValue(params.get(3).getValueAsBoolean());
								allowBackSlash.setValue(params.get(4).getValueAsBoolean());
								allowUrlEncodedPercent.setValue(params.get(5).getValueAsBoolean());
								allowUrlEncodedSlash.setValue(params.get(6).getValueAsBoolean());
								allowUrlEncodedPeriod.setValue(params.get(7).getValueAsBoolean());
							}
						});
	}

	private ToggleItem prepareAllowUrlEncodedPeriodSwitch() {
		ToggleItem allowUrlEncodedPeriod = ItemFactory.newToggleItem("allowUrlEncodedPeriod", "allowurlencodedperiod",
				Session.get().getConfigAsBoolean(FIREWALL_ALLOW_URL_ENCODED_PERIOD));
		allowUrlEncodedPeriod.setWrapTitle(false);
		allowUrlEncodedPeriod.setRequired(true);
		allowUrlEncodedPeriod.setDisabled(Session.get().isDemo());
		if (changedHandler != null)
			allowUrlEncodedPeriod.addChangedHandler(changedHandler);
		return allowUrlEncodedPeriod;
	}

	private ToggleItem prepareAllowUrlEncodedSlashSwitch() {
		ToggleItem allowUrlEncodedSlash = ItemFactory.newToggleItem("allowUrlEncodedSlash", "allowurlencodedslash",
				Session.get().getConfigAsBoolean(FIREWALL_ALLOW_URL_ENCODED_SLASH));
		allowUrlEncodedSlash.setWrapTitle(false);
		allowUrlEncodedSlash.setRequired(true);
		allowUrlEncodedSlash.setDisabled(Session.get().isDemo());
		if (changedHandler != null)
			allowUrlEncodedSlash.addChangedHandler(changedHandler);
		return allowUrlEncodedSlash;
	}

	private ToggleItem prepareAllowUrlEncodedPercentSwitch() {
		ToggleItem allowUrlEncodedPercent = ItemFactory.newToggleItem("allowUrlEncodedPercent",
				"allowurlencodedpercent", Session.get().getConfigAsBoolean(FIREWALL_ALLOW_URL_ENCODED_PERCENT));
		allowUrlEncodedPercent.setWrapTitle(false);
		allowUrlEncodedPercent.setRequired(true);
		allowUrlEncodedPercent.setDisabled(Session.get().isDemo());
		if (changedHandler != null)
			allowUrlEncodedPercent.addChangedHandler(changedHandler);
		return allowUrlEncodedPercent;
	}

	private ToggleItem prepareAllowBackSlashSwitch() {
		ToggleItem allowBackSlash = ItemFactory.newToggleItem("allowBackSlash", "allowbackslash",
				Session.get().getConfigAsBoolean(FIREWALL_ALLOW_BACK_SLASH));
		allowBackSlash.setWrapTitle(false);
		allowBackSlash.setRequired(true);
		allowBackSlash.setDisabled(Session.get().isDemo());
		if (changedHandler != null)
			allowBackSlash.addChangedHandler(changedHandler);
		return allowBackSlash;
	}

	private ToggleItem prepareAllowSemicolonSwitch() {
		ToggleItem allowSemicolon = ItemFactory.newToggleItem("allowSemicolon", "allowsemicolon",
				Session.get().getConfigAsBoolean(FIREWALL_ALLOW_SEMICOLON));
		allowSemicolon.setWrapTitle(false);
		allowSemicolon.setRequired(true);
		allowSemicolon.setDisabled(Session.get().isDemo());
		if (changedHandler != null)
			allowSemicolon.addChangedHandler(changedHandler);
		return allowSemicolon;
	}

	private TextAreaItem prepareBlackListItem() {
		final TextAreaItem blacklist = ItemFactory.newTextAreaItem(BLACKLIST, null);
		blacklist.setHeight(120);
		blacklist.setWidth(350);
		blacklist.setHint(I18N.message("blacklisthint"));
		blacklist.setDisabled(Session.get().isDemo());
		if (changedHandler != null)
			blacklist.addChangedHandler(changedHandler);
		return blacklist;
	}

	private TextAreaItem prepareWhiteListItem() {
		final TextAreaItem whitelist = ItemFactory.newTextAreaItem(WHITELIST, null);
		whitelist.setHeight(120);
		whitelist.setWidth(350);
		whitelist.setHint(I18N.message("blacklisthint"));
		whitelist.setDisabled(Session.get().isDemo());
		if (changedHandler != null)
			whitelist.addChangedHandler(changedHandler);
		return whitelist;
	}

	private ToggleItem prepareEnabledSwitch() {
		ToggleItem enabled = ItemFactory.newToggleItem("eenabled", "enabled",
				Session.get().getConfigAsBoolean(FIREWALL_ENABLED));
		enabled.setWrapTitle(false);
		enabled.setRequired(true);
		enabled.setDisabled(Session.get().isDemo());
		if (changedHandler != null)
			enabled.addChangedHandler(changedHandler);
		return enabled;
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setWidth(1);
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.LEFT);
		if (user != null)
			form.setNumCols(2);
		else
			form.setNumCols(1);
	}

	public void onSave() {
		String enabled = vm.getValueAsString("eenabled");
		String whitelist = vm.getValueAsString(WHITELIST);
		String blacklist = vm.getValueAsString(BLACKLIST);

		List<GUIParameter> params = new ArrayList<>();

		params.add(new GUIParameter(FIREWALL_ENABLED, enabled));
		params.add(new GUIParameter(FIREWALL_WHITELIST,
				whitelist != null ? whitelist.replace('\n', ',').replace(" ", "") : null));
		params.add(new GUIParameter(FIREWALL_BLACKLIST,
				blacklist != null ? blacklist.replace('\n', ',').replace(" ", "") : null));
		params.add(new GUIParameter(FIREWALL_ALLOW_SEMICOLON, vm.getValueAsString("allowSemicolon")));
		params.add(new GUIParameter(FIREWALL_ALLOW_BACK_SLASH, vm.getValueAsString("allowBackSlash")));
		params.add(new GUIParameter(FIREWALL_ALLOW_URL_ENCODED_PERCENT, vm.getValueAsString("allowUrlEncodedPercent")));
		params.add(new GUIParameter(FIREWALL_ALLOW_URL_ENCODED_SLASH, vm.getValueAsString("allowUrlEncodedSlash")));
		params.add(new GUIParameter(FIREWALL_ALLOW_URL_ENCODED_PERIOD, vm.getValueAsString("allowUrlEncodedPeriod")));

		for (GUIParameter guiParameter : params)
			Session.get().setConfig(guiParameter.getName(), guiParameter.getValue());

		SettingService.Instance.get().saveFirewallSettings(params, new AsyncCallback<>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg) {
				if (user == null)
					GuiLog.info(I18N.message("settingssaved"), null);
			}
		});
	}

	public boolean validate() {
		String whitelist = vm.getValueAsString(WHITELIST);
		String blacklist = vm.getValueAsString(BLACKLIST);
		user.setIpWhitelist(whitelist != null ? whitelist.replace('\n', ',').replace(" ", "") : null);
		user.setIpBlacklist(blacklist != null ? blacklist.replace('\n', ',').replace(" ", "") : null);
		return true;
	}
}