package com.logicaldoc.gui.frontend.client.security;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.util.SC;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel shows the Sinlge-signon settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class SingleSignonPanel extends VLayout {

	private static final String GROUP = "group";

	private static final String ENABLED = "enabled";

	private static final String CAS_APPURL = "cas.appurl";

	private static final String CAS_LOGINURL = "cas.loginurl";

	private static final String CAS_GROUP = "cas.group";

	private static final String CAS_LANG = "cas.lang";

	private static final String CAS_USER_TYPE = "cas.userType";

	private static final String CAS_URL = "cas.url";

	private static final String CAS_ID = "cas.id";

	private static final String CAS_ENABLED = "cas.enabled";

	public SingleSignonPanel() {
		setWidth100();
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	protected void onDraw() {
		SettingService.Instance.get().loadSettingsByNames(new String[] { CAS_ENABLED, CAS_ID, CAS_URL, CAS_APPURL,
				CAS_LOGINURL, CAS_GROUP, CAS_LANG, CAS_USER_TYPE }, new AsyncCallback<GUIParameter[]>() {
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
		final DynamicForm form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setAlign(Alignment.LEFT);

		RadioGroupItem enabled = ItemFactory.newBooleanSelector(ENABLED, I18N.message("enablecas"));
		enabled.setWrapTitle(false);
		enabled.setRequired(true);

		TextItem id = ItemFactory.newTextItem("id", "applicationid", null);
		id.setWidth(200);
		id.setWrapTitle(false);
		id.setRequired(true);

		TextItem appUrl = ItemFactory.newTextItem("appurl", "applicationurl", null);
		appUrl.setWidth(400);
		appUrl.setWrapTitle(false);
		appUrl.setRequired(true);

		TextItem casUrl = ItemFactory.newTextItem("casurl", null);
		casUrl.setWidth(400);
		casUrl.setWrapTitle(false);
		casUrl.setRequired(true);

		TextItem casLoginUrl = ItemFactory.newTextItem("loginurl", "casloginurl", null);
		casLoginUrl.setWidth(400);
		casLoginUrl.setWrapTitle(false);
		casLoginUrl.setRequired(true);

		SelectItem group = ItemFactory.newGroupSelector(GROUP, GROUP);
		group.setHint(I18N.message("casgrouphint"));
		group.setWrapTitle(false);
		group.setRequired(true);

		SelectItem language = ItemFactory.newLanguageSelector("language", false, true);
		language.setHint(I18N.message("caslanghint"));
		language.setWrapTitle(false);
		language.setRequired(true);

		SelectItem userType = ItemFactory.newUserTypeSelector("usertype", GUIUser.TYPE_DEFAULT);
		userType.setHint(I18N.message("casusertypehint"));
		userType.setWrapTitle(false);
		userType.setRequired(true);

		initValues(settings, enabled, id, appUrl, casUrl, casLoginUrl, group, language, userType);

		IButton save = prepareSaveButton(form);

		form.setFields(enabled, id, appUrl, casUrl, casLoginUrl, group, language, userType);

		Tab tab = new Tab();
		tab.setTitle(I18N.message("singlesignon"));
		tab.setPane(form);

		TabSet tabs = new TabSet();
		tabs.setWidth100();
		tabs.setHeight100();
		tabs.setTabs(tab);

		setMembers(tabs, save);
	}

	private void initValues(GUIParameter[] settings, RadioGroupItem enabled, TextItem id, TextItem appUrl,
			TextItem casUrl, TextItem casLoginUrl, SelectItem group, SelectItem language, SelectItem userType) {
		for (GUIParameter setting : settings) {
			if (CAS_ENABLED.equals(setting.getName()))
				enabled.setValue("true".equals(setting.getValue()) ? "yes" : "no");
			else if (CAS_ID.equals(setting.getName()))
				id.setValue(setting.getValue());
			else if (CAS_LOGINURL.equals(setting.getName()))
				casLoginUrl.setValue(setting.getValue());
			else if (CAS_APPURL.equals(setting.getName()))
				appUrl.setValue(setting.getValue());
			else if (CAS_URL.equals(setting.getName()))
				casUrl.setValue(setting.getValue());
			else if (CAS_GROUP.equals(setting.getName()))
				group.setValue(setting.getValue());
			else if (CAS_LANG.equals(setting.getName()))
				language.setValue(setting.getValue());
			else if (CAS_USER_TYPE.equals(setting.getName()))
				userType.setValue(setting.getValue());
		}
	}

	private IButton prepareSaveButton(final DynamicForm form) {
		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler((ClickEvent event) -> {
			if (!form.validate())
				return;

			GUIParameter[] params = new GUIParameter[8];
			params[0] = new GUIParameter(CAS_ENABLED, "" + ("yes".equals(form.getValueAsString(ENABLED))));
			params[1] = new GUIParameter(CAS_ID, form.getValueAsString("id").trim());
			params[2] = new GUIParameter(CAS_URL, form.getValueAsString("casurl").trim());
			params[3] = new GUIParameter(CAS_APPURL, form.getValueAsString("appurl").trim());
			params[4] = new GUIParameter(CAS_LOGINURL, form.getValueAsString("loginurl").trim());
			params[5] = new GUIParameter(CAS_GROUP, form.getValueAsString(GROUP).trim());
			params[6] = new GUIParameter(CAS_LANG, form.getValueAsString("language").trim());
			params[7] = new GUIParameter(CAS_USER_TYPE, form.getValueAsString("usertype").trim());

			for (GUIParameter param : params)
				Session.get().setConfig(param.getName(), param.getValue());

			SettingService.Instance.get().saveSettings(params, new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void ret) {
					GuiLog.info(I18N.message("settingssaved"), null);
					if ("yes".equals(form.getValueAsString(ENABLED)))
						SC.say(I18N.message("settingssaved") + "\n" + I18N.message("suggestedtorestart"));
				}
			});
		});
		return save;
	}
}