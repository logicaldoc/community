package com.logicaldoc.gui.frontend.client.security.saml;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.CopyTextFormItemIcon;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;

/**
 * This panel shows the Saml settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class SamlPanel extends VLayout {

	private static final String SAML_ENABLED = "saml.enabled";

	private static final String SAML_SP_ENTITYID = "saml.sp.entityid";

	private static final String SAML_SP_X509CERT = "saml.sp.x509cert";

	private static final String SAML_SP_PRIVATEKEY = "saml.sp.privatekey";

	public SamlPanel() {
		setWidth100();
		setMembersMargin(5);
		setMargin(5);
	}

	@Override
	protected void onDraw() {
		SettingService.Instance.get().loadSettingsByNames(
				new String[] { SAML_ENABLED, SAML_SP_ENTITYID, SAML_SP_X509CERT, SAML_SP_PRIVATEKEY },
				new AsyncCallback<GUIParameter[]>() {
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
		
		RadioGroupItem enabled = ItemFactory.newBooleanSelector(SAML_ENABLED, I18N.message("enabled"));
		enabled.setWrapTitle(false);
		enabled.setRequired(true);
		
		TextItem id = ItemFactory.newTextItem(SAML_SP_ENTITYID, "entityid", null);
		id.setWidth(300);
		id.setWrapTitle(false);
		id.setRequired(true);
	
		TextItem certificate = ItemFactory.newTextItem(SAML_SP_X509CERT, "x509cert", null);
		certificate.setWidth(300);
		certificate.setWrapTitle(false);
		certificate.setRequired(true);
		certificate.setIcons(new CopyTextFormItemIcon());

		TextItem privateKey = ItemFactory.newTextItem(SAML_SP_PRIVATEKEY, "privatekey", null);
		privateKey.setWidth(300);
		privateKey.setWrapTitle(false);
		privateKey.setRequired(true);
		certificate.setIcons(new CopyTextFormItemIcon());

		form.setFields(enabled, id, certificate, privateKey);
		
		GuiLog.info("A");
		for (GUIParameter setting : settings) {
			GuiLog.info("B "+setting.getName());
			if (SAML_ENABLED.equals(setting.getName()))
				form.getItem(setting.getName()).setValue("true".equals(setting.getValue()) ? "yes" : "no");
			else
				form.getItem(setting.getName()).setValue(setting.getValue());
		}

		GuiLog.info("C");
		
		IButton save = prepareSaveButton(form);

		

		Tab tab = new Tab();
		tab.setTitle(I18N.message("singlesignonsaml"));
		tab.setPane(form);

		TabSet tabs = new TabSet();
		tabs.setWidth100();
		tabs.setHeight100();
		tabs.setTabs(tab);

		setMembers(tabs, save);
	}

	private IButton prepareSaveButton(final DynamicForm form) {
		IButton save = new IButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler((ClickEvent event) -> {
			if (!form.validate())
				return;

			List<GUIParameter> params = new ArrayList<>();
			@SuppressWarnings("unchecked")
			Map<String, Object> values = (Map<String, Object>) form.getValues();
			for (Map.Entry<String, Object> entry : values.entrySet())
				params.add(
						new GUIParameter(entry.getKey(), entry.getValue() != null ? entry.getValue().toString() : ""));

			for (GUIParameter param : params)
				Session.get().setConfig(param.getName(), param.getValue());

			SettingService.Instance.get().saveSettings(params.toArray(new GUIParameter[0]), new AsyncCallback<Void>() {

				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(Void ret) {
					GuiLog.info(I18N.message("settingssaved"), null);
				}
			});
		});
		return save;
	}
}