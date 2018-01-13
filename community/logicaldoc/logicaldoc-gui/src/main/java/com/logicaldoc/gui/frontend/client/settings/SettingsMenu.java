package com.logicaldoc.gui.frontend.client.settings;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Menu;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIEmailSettings;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.beans.GUISearchEngine;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.frontend.client.administration.AdminScreen;
import com.logicaldoc.gui.frontend.client.services.SearchEngineService;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.logicaldoc.gui.frontend.client.settings.converters.FormatConvertersPanel;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This panel shows the administration system configurations menu
 * 
 * @author Matteo Caruso - Logical Objects
 * @since 6.0
 */
public class SettingsMenu extends VLayout {

	public SettingsMenu() {
		setMargin(10);
		setMembersMargin(5);

		Button searchAndIndexing = new Button(I18N.message("searchandindexing"));
		searchAndIndexing.setWidth100();
		searchAndIndexing.setHeight(25);
		addMember(searchAndIndexing);

		Button repositories = new Button(I18N.message("repositories"));
		repositories.setWidth100();
		repositories.setHeight(25);
		if (Session.get().isDefaultTenant() && Menu.enabled(Menu.REPOSITORIES))
			addMember(repositories);

		Button guiSettings = new Button(I18N.message("guisettings"));
		guiSettings.setWidth100();
		guiSettings.setHeight(25);
		addMember(guiSettings);

		Button quota = new Button(I18N.message("quota"));
		quota.setWidth100();
		quota.setHeight(25);

		Button keystore = new Button(I18N.message("keystore"));
		keystore.setWidth100();
		keystore.setHeight(25);

		Button converters = new Button(I18N.message("formatconverters"));
		converters.setWidth100();
		converters.setHeight(25);

		Button ocr = new Button(I18N.message("ocr"));
		ocr.setWidth100();
		ocr.setHeight(25);
		if (Feature.visible(Feature.OCR)) {
			addMember(ocr);
			if (!Feature.enabled(Feature.OCR)) {
				ocr.setDisabled(true);
				ocr.setTooltip(I18N.message("featuredisabled"));
			}
		}

		Button smtp = new Button(I18N.message("outgoingemail"));
		smtp.setWidth100();
		smtp.setHeight(25);
		if (Session.get().isDemo()) {
			smtp.setDisabled(true);
			smtp.setTooltip(I18N.message("featuredisabled"));
		}

		Button clientTools = new Button(I18N.message("clienandextapps"));
		clientTools.setWidth100();
		clientTools.setHeight(25);

		Button parameters = new Button(I18N.message("parameters"));
		parameters.setWidth100();
		parameters.setHeight(25);

		if (Feature.visible(Feature.CLIENT_TOOLS) && Menu.enabled(Menu.CLIENTS)) {
			addMember(clientTools);
			if (!Feature.enabled(Feature.CLIENT_TOOLS)) {
				clientTools.setDisabled(true);
				clientTools.setTooltip(I18N.message("featuredisabled"));
			}
		}

		clientTools.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SettingService.Instance.get().loadClientSettings(new AsyncCallback<GUIParameter[]>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIParameter[] settings) {
						AdminScreen.get().setContent(new ExternalAppsPanel(settings));
					}

				});
			}
		});

		quota.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new QuotaPanel(Session.get().getTenantId()));
			}
		});

		keystore.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new KeystorePanel());
			}
		});

		if (Session.get().isDefaultTenant() && Feature.visible(Feature.FORMAT_CONVERSION)
				&& Menu.enabled(Menu.FORMAT_CONVERTERS)) {
			addMember(converters);
			if (!Feature.enabled(Feature.FORMAT_CONVERSION)) {
				converters.setDisabled(true);
				converters.setTooltip(I18N.message("featuredisabled"));
			}
		}

		addMember(smtp);

		if (Session.get().isDefaultTenant() && Menu.enabled(Menu.QUOTA))
			addMember(quota);

		if (Feature.enabled(Feature.DIGITAL_SIGNATURE) && Menu.enabled(Menu.KEYSTORE))
			addMember(keystore);

		if (Session.get().isDefaultTenant() && Menu.enabled(Menu.PARAMETERS))
			addMember(parameters);

		searchAndIndexing.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SearchEngineService.Instance.get().getInfo(new AsyncCallback<GUISearchEngine>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUISearchEngine searchEngine) {
						AdminScreen.get().setContent(new SearchIndexingPanel(searchEngine));
					}

				});
			}
		});

		guiSettings.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SettingService.Instance.get().loadGUISettings(new AsyncCallback<GUIParameter[]>() {
					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIParameter[] settings) {
						AdminScreen.get().setContent(new GUISettingsPanel(settings));
					}
				});
			}
		});

		ocr.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SettingService.Instance.get().loadOcrSettings(new AsyncCallback<GUIParameter[]>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIParameter[] settings) {
						AdminScreen.get().setContent(new OCRSettingsPanel(settings));
					}
				});
			}
		});

		parameters.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SettingService.Instance.get().loadSettings(new AsyncCallback<GUIParameter[]>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIParameter[] settings) {
						AdminScreen.get().setContent(new ParametersPanel(settings));
					}

				});
			}
		});

		smtp.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				SettingService.Instance.get().loadEmailSettings(new AsyncCallback<GUIEmailSettings>() {

					@Override
					public void onFailure(Throwable caught) {
						Log.serverError(caught);
					}

					@Override
					public void onSuccess(GUIEmailSettings settings) {
						AdminScreen.get().setContent(new OutgoingEmailPanel(settings));
					}

				});
			}
		});

		repositories.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new RepositoriesPanel());
			}
		});

		converters.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				AdminScreen.get().setContent(new FormatConvertersPanel());
			}
		});
	}
}