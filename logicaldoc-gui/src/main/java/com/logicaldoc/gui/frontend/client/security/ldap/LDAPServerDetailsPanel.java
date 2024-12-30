package com.logicaldoc.gui.frontend.client.security.ldap;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.beans.GUILDAPServer;
import com.logicaldoc.gui.common.client.data.GroupsDS;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FeatureDisabled;
import com.logicaldoc.gui.frontend.client.services.LDAPService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.PasswordItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel shows the LDAP and Active Directory settings.
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.0
 */
public class LDAPServerDetailsPanel extends VLayout {

	private static final String GRPSBASENODE = "grpsbasenode";

	private static final String USERSBASENODE = "usersbasenode";

	private static final String PASSWORD_HIDDEN = "password_hidden";

	private static final String KEEPMEMBERSHIP = "keepmembership";

	private static final String VALIDATION = "validation";

	private static final String LANGUAGE = "language";

	private static final String GROUPEXCLUDE = "groupexclude";

	private static final String GROUPINCLUDE = "groupinclude";

	private static final String USEREXCLUDE = "userexclude";

	private static final String USERINCLUDE = "userinclude";

	private static final String GRPCLASS = "grpclass";

	private static final String USERCLASS = "userclass";

	private static final String LOGONATTR = "logonattr";

	private static final String GRPIDENTIFIERATTR = "grpidentifierattr";

	private static final String USERIDENTIFIERATTR = "useridentifierattr";

	private static final String REALM = "realm";

	private static final String USERNAME = "username";

	private static final String EENABLED = "eenabled";

	private ValuesManager vm = new ValuesManager();

	private MultiComboBoxItem defaultGroupsItem;

	private TabSet tabs = new TabSet();

	private GUILDAPServer server;

	private LDAPBrowser browser;

	public LDAPServerDetailsPanel(LDAPServersPanel listing, GUILDAPServer server) {
		this.server = server;

		setWidth100();
		setMembersMargin(5);
		setMargin(5);

		tabs.setWidth100();
		tabs.setHeight100();

		Tab ldapTab = new Tab();
		ldapTab.setTitle(I18N.message("server"));

		Tab browserTab = new Tab();
		browserTab.setTitle(I18N.message("browser"));

		DynamicForm ldapForm = new DynamicForm();
		ldapForm.setValuesManager(vm);
		ldapForm.setTitleOrientation(TitleOrientation.TOP);
		ldapForm.setColWidths(100, 100);

		// Enabled
		ToggleItem enabled = ItemFactory.newToggleItem(EENABLED, "enabled", this.server.isEnabled());
		enabled.setCellStyle("warn");
		enabled.setRequired(true);

		// Anonymous Login
		ToggleItem anon = ItemFactory.newToggleItem("anon", "anonymous", this.server.isAnonymous());
		anon.setRequired(true);

		// Url
		TextItem url = ItemFactory.newTextItem("url", "ldaphurl", this.server.getUrl());
		url.setRequired(true);
		url.setCellStyle("warn");
		url.setWidth(300);

		// Username
		TextItem username = ItemFactory.newTextItemPreventAutocomplete(USERNAME, "user", this.server.getUsername());
		username.setCellStyle("warn");
		username.setWidth(300);

		// User type
		SelectItem userType = ItemFactory.newUserTypeSelector("usertype", this.server.getUserType());

		// Realm
		TextItem realm = ItemFactory.newTextItem(REALM, this.server.getRealm());
		realm.setWidth(300);

		// User identifier attr.
		TextItem userIdentifierAttr = ItemFactory.newTextItem(USERIDENTIFIERATTR, this.server.getUserIdentifierAttr());
		userIdentifierAttr.setWidth(300);

		// Group identifier attr.
		TextItem grpIdentifierAttr = ItemFactory.newTextItem(GRPIDENTIFIERATTR, this.server.getGroupIdentifierAttr());
		grpIdentifierAttr.setWidth(300);

		// Logon attr.
		TextItem logonAttr = ItemFactory.newTextItem(LOGONATTR, this.server.getLogonAttr());
		logonAttr.setWidth(300);

		// User class
		TextItem userClass = ItemFactory.newTextItem(USERCLASS, this.server.getUserClass());
		userClass.setWidth(300);

		// Group class
		TextItem groupClass = ItemFactory.newTextItem(GRPCLASS, this.server.getGroupClass());
		groupClass.setWidth(300);

		// Users base node
		TextItem usersBaseNode = ItemFactory.newTextItem(USERSBASENODE, this.server.getUserNodes());
		usersBaseNode.setWidth(300);

		// User filters
		TextItem userInclude = ItemFactory.newTextItem(USERINCLUDE, "userinclusionfilers",
				this.server.getUserIncludes());
		userInclude.setWidth(300);
		TextItem userExclude = ItemFactory.newTextItem(USEREXCLUDE, "userexclusionfilers",
				this.server.getUserExcludes());
		userExclude.setWidth(300);

		// Groups base node
		TextItem groupsBaseNode = ItemFactory.newTextItem(GRPSBASENODE, this.server.getGroupNodes());
		groupsBaseNode.setWidth(300);

		// Group filters
		TextItem groupInclude = ItemFactory.newTextItem(GROUPINCLUDE, "groupinclusionfilers",
				this.server.getGroupIncludes());
		groupInclude.setWidth(300);
		TextItem groupExclude = ItemFactory.newTextItem(GROUPEXCLUDE, "groupexclusionfilers",
				this.server.getGroupExcludes());
		groupExclude.setWidth(300);

		// Page size
		SpinnerItem pageSize = ItemFactory.newSpinnerItem("pagesize", this.server.getPageSize());
		pageSize.setRequired(true);
		pageSize.setMin(0);
		pageSize.setStep(50);

		// Timepout
		SpinnerItem timeout = ItemFactory.newSpinnerItem("timeout", this.server.getTimeout());
		timeout.setRequired(true);
		timeout.setMin(1);
		timeout.setStep(5);
		timeout.setHint(I18N.message("seconds").toLowerCase());

		// Sync TTL
		SpinnerItem syncTtl = ItemFactory.newSpinnerItem("syncttl", "synchronizeifolderthan", this.server.getSyncTtl());
		syncTtl.setRequired(true);
		syncTtl.setHint(I18N.message("hours").toLowerCase());
		syncTtl.setMin(-1);
		syncTtl.setStep(1);

		// Language
		SelectItem language = ItemFactory.newLanguageSelector(LANGUAGE, false, true);
		language.setName(LANGUAGE);
		language.setRequired(true);
		language.setValue(this.server.getLanguage());

		// Keep membership in local groups
		ToggleItem keepMembership = ItemFactory.newToggleItem(KEEPMEMBERSHIP, "keepmembershiplocalgroups",
				this.server.isKeepLocalMemberships());
		keepMembership.setRequired(true);

		TextAreaItem validation = ItemFactory.newTextAreaItemForAutomation(VALIDATION, this.server.getValidation(),
				null, false);
		validation.setHeight(150);
		validation.setWidth(400);
		validation.setWrapTitle(false);
		validation.setColSpan(2);

		/*
		 * Two invisible fields to 'mask' the real credentials to the browser
		 * and prevent it to auto-fill the username and password we really use.
		 */
		TextItem fakeUsername = ItemFactory.newTextItem("prevent_autofill", this.server.getUsername());
		fakeUsername.setCellStyle("nodisplay");
		PasswordItem hiddenPassword = ItemFactory.newPasswordItem(PASSWORD_HIDDEN, PASSWORD_HIDDEN,
				this.server.getPassword());
		hiddenPassword.setCellStyle("nodisplay");

		// Password
		FormItem password = ItemFactory.newSafePasswordItem("password", I18N.message("password"),
				this.server.getPassword(), hiddenPassword, null);
		password.setCellStyle("warn");
		password.setWidth(300);

		// Default groups

		defaultGroupsItem = ItemFactory.newMultiComboBoxItem("defaultGroups", "defaultassignedgroups", new GroupsDS(),
				server.getDefaultGroups().stream().map(g -> g.getId()).collect(Collectors.toList())
						.toArray(new Long[0]));
		defaultGroupsItem.setValueField("id");
		defaultGroupsItem.setDisplayField("name");

		ldapForm.setItems(enabled, url, fakeUsername, hiddenPassword, username, password, anon, pageSize, timeout,
				language, userType, defaultGroupsItem, syncTtl, keepMembership, userIdentifierAttr, grpIdentifierAttr,
				userClass, groupClass, usersBaseNode, groupsBaseNode, userInclude, groupInclude, userExclude,
				groupExclude, logonAttr, realm, validation);

		ldapTab.setPane(ldapForm);

		if (Feature.visible(Feature.LDAP)) {
			tabs.addTab(ldapTab);
			tabs.addTab(browserTab);
			if (!Feature.enabled(Feature.LDAP)) {
				ldapTab.setPane(new FeatureDisabled());
				browserTab.setPane(new FeatureDisabled());
			} else {
				ldapTab.setPane(ldapForm);
				browserTab.setPane(new LDAPBrowser(server));
			}
		}

		IButton test = prepareTestButton(listing, server);

		IButton activedir = prepareActiveDirButton();

		IButton save = prepareSaveButton(listing, test);

		HLayout buttons = new HLayout();
		buttons.setMembersMargin(3);
		buttons.setMembers(save, activedir, test);
		setMembers(tabs, buttons);
	}

	private IButton prepareSaveButton(LDAPServersPanel listing, IButton test) {
		IButton save = new IButton();
		save.setAutoFit(true);
		save.setTitle(I18N.message("save"));
		save.addClickHandler(click -> {

			if (Boolean.FALSE.equals(vm.validate()))
				return;

			LDAPServerDetailsPanel.this.server.setEnabled(Boolean.valueOf(vm.getValueAsString(EENABLED)));
			LDAPServerDetailsPanel.this.server.setAnonymous(Boolean.valueOf(vm.getValueAsString("anon")));
			LDAPServerDetailsPanel.this.server
					.setKeepLocalMemberships(Boolean.valueOf(vm.getValueAsString(KEEPMEMBERSHIP)));
			LDAPServerDetailsPanel.this.server.setUrl(vm.getValueAsString("url"));
			LDAPServerDetailsPanel.this.server.setUsername(vm.getValueAsString(USERNAME));
			LDAPServerDetailsPanel.this.server.setRealm(vm.getValueAsString(REALM));
			LDAPServerDetailsPanel.this.server.setUserIdentifierAttr(vm.getValueAsString(USERIDENTIFIERATTR));
			LDAPServerDetailsPanel.this.server.setGroupIdentifierAttr(vm.getValueAsString(GRPIDENTIFIERATTR));
			LDAPServerDetailsPanel.this.server.setLogonAttr(vm.getValueAsString(LOGONATTR));
			LDAPServerDetailsPanel.this.server.setUserClass(vm.getValueAsString(USERCLASS));
			LDAPServerDetailsPanel.this.server.setGroupClass(vm.getValueAsString(GRPCLASS));
			LDAPServerDetailsPanel.this.server.setUserNodes(vm.getValueAsString(USERSBASENODE));
			LDAPServerDetailsPanel.this.server.setUserIncludes(vm.getValueAsString(USERINCLUDE));
			LDAPServerDetailsPanel.this.server.setUserExcludes(vm.getValueAsString(USEREXCLUDE));
			LDAPServerDetailsPanel.this.server.setGroupNodes(vm.getValueAsString(GRPSBASENODE));
			LDAPServerDetailsPanel.this.server.setGroupIncludes(vm.getValueAsString(GROUPINCLUDE));
			LDAPServerDetailsPanel.this.server.setGroupExcludes(vm.getValueAsString(GROUPEXCLUDE));
			LDAPServerDetailsPanel.this.server.setPageSize(Integer.parseInt(vm.getValueAsString("pagesize")));
			LDAPServerDetailsPanel.this.server.setSyncTtl(Integer.parseInt(vm.getValueAsString("syncttl")));
			LDAPServerDetailsPanel.this.server.setLanguage(vm.getValueAsString(LANGUAGE));
			LDAPServerDetailsPanel.this.server.setUserType(Integer.parseInt(vm.getValueAsString("usertype")));
			LDAPServerDetailsPanel.this.server.setValidation(vm.getValueAsString(VALIDATION));
			LDAPServerDetailsPanel.this.server.setTimeout(Integer.parseInt(vm.getValueAsString("timeout")));

			LDAPServerDetailsPanel.this.server.setPassword(vm.getValueAsString(PASSWORD_HIDDEN));

			String[] ids = defaultGroupsItem.getValues();
			List<GUIGroup> groups = new ArrayList<>();
			for (int i = 0; i < ids.length; i++) {
				GUIGroup group = new GUIGroup();
				group.setId(Long.parseLong(ids[i]));
				groups.add(group);
			}
			LDAPServerDetailsPanel.this.server.setDefaultGroups(groups);

			LDAPService.Instance.get().save(LDAPServerDetailsPanel.this.server, new AsyncCallback<>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUILDAPServer server) {
					LDAPServerDetailsPanel.this.server = server;
					if (browser instanceof LDAPBrowser)
						browser.setServer(LDAPServerDetailsPanel.this.server);
					listing.updateRecord(LDAPServerDetailsPanel.this.server);
					test.setDisabled(false);
					GuiLog.info(I18N.message("settingssaved"), null);
				}
			});
		});
		return save;
	}

	private IButton prepareActiveDirButton() {
		IButton activedir = new IButton();
		activedir.setAutoFit(true);
		activedir.setTitle(I18N.message("activedirectory"));
		activedir.addClickHandler((ClickEvent event) -> LD.askForValue(I18N.message("activedirectory"),
				I18N.message("addomain"), "", (String value) -> {
					if (value == null)
						return;
					String node = value.replace("\\.", ",DC=");
					node = "DC=" + node;
					vm.setValue("url", "ldap://AD_SERVER:389");
					vm.setValue(USERNAME, "CN=Administrator,CN=Users," + node);
					vm.setValue(USERIDENTIFIERATTR, "CN");
					vm.setValue(GRPIDENTIFIERATTR, "CN");
					vm.setValue(LOGONATTR, "sAMAccountName");
					vm.setValue(USERCLASS, "person");
					vm.setValue(GRPCLASS, "group");
					vm.setValue(USERCLASS, "person");
					vm.setValue(USERSBASENODE, "CN=Users," + node);
					vm.setValue(GRPSBASENODE, "CN=Builtin," + node);
					vm.setValue("anon", "no");
				}));
		return activedir;
	}

	private IButton prepareTestButton(LDAPServersPanel listing, GUILDAPServer server) {
		IButton test = new IButton();
		test.setAutoFit(true);
		test.setTitle(I18N.message("testconnection"));
		test.setDisabled(server.getId() == 0L);
		test.addClickHandler((ClickEvent event) -> {
			if (Boolean.FALSE.equals(vm.validate()))
				return;

			LDAPServerDetailsPanel.this.server.setEnabled(Boolean.valueOf(vm.getValueAsString(EENABLED)));
			LDAPServerDetailsPanel.this.server.setAnonymous(Boolean.valueOf(vm.getValueAsString("anon")));
			LDAPServerDetailsPanel.this.server
					.setKeepLocalMemberships(Boolean.valueOf(vm.getValueAsString(KEEPMEMBERSHIP)));
			LDAPServerDetailsPanel.this.server.setUrl(vm.getValueAsString("url"));
			LDAPServerDetailsPanel.this.server.setUsername(vm.getValueAsString(USERNAME));
			LDAPServerDetailsPanel.this.server.setRealm(vm.getValueAsString(REALM));
			LDAPServerDetailsPanel.this.server.setUserIdentifierAttr(vm.getValueAsString(USERIDENTIFIERATTR));
			LDAPServerDetailsPanel.this.server.setGroupIdentifierAttr(vm.getValueAsString(GRPIDENTIFIERATTR));
			LDAPServerDetailsPanel.this.server.setLogonAttr(vm.getValueAsString(LOGONATTR));
			LDAPServerDetailsPanel.this.server.setUserClass(vm.getValueAsString(USERCLASS));
			LDAPServerDetailsPanel.this.server.setGroupClass(vm.getValueAsString(GRPCLASS));
			LDAPServerDetailsPanel.this.server.setUserNodes(vm.getValueAsString(USERSBASENODE));
			LDAPServerDetailsPanel.this.server.setUserIncludes(vm.getValueAsString(USERINCLUDE));
			LDAPServerDetailsPanel.this.server.setUserExcludes(vm.getValueAsString(USEREXCLUDE));
			LDAPServerDetailsPanel.this.server.setGroupNodes(vm.getValueAsString(GRPSBASENODE));
			LDAPServerDetailsPanel.this.server.setGroupIncludes(vm.getValueAsString(GROUPINCLUDE));
			LDAPServerDetailsPanel.this.server.setGroupExcludes(vm.getValueAsString(GROUPEXCLUDE));
			LDAPServerDetailsPanel.this.server.setLanguage(vm.getValueAsString(LANGUAGE));
			LDAPServerDetailsPanel.this.server.setValidation(vm.getValueAsString(VALIDATION));

			LDAPServerDetailsPanel.this.server.setPassword(vm.getValueAsString(PASSWORD_HIDDEN));

			if (browser instanceof LDAPBrowser)
				browser.setServer(LDAPServerDetailsPanel.this.server);

			listing.updateRecord(LDAPServerDetailsPanel.this.server);

			LDAPService.Instance.get().testConnection(LDAPServerDetailsPanel.this.server, new AsyncCallback<Boolean>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Boolean ret) {
					if (Boolean.TRUE.equals(ret))
						SC.say(I18N.message("connectionestablished"));
					else
						SC.warn(I18N.message("connectionfailed"));
				}
			});
		});
		return test;
	}
}