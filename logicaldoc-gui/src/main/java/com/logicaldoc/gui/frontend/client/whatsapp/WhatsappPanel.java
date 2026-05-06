package com.logicaldoc.gui.frontend.client.whatsapp;

import java.util.ArrayList;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;

/**
 * The Whatsapp configuration.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class WhatsappPanel extends AdminPanel {

    public WhatsappPanel() {
        super("auditing");

        WhatsappService.Instance.get().loadSettings(new DefaultAsyncCallback<>() {
            @Override
            public void handleSuccess(List<String> settings) {
                initGUI(settings);
            }
        });
    }

    private void initGUI(List<String> settings) {
        TextItem accountId = ItemFactory.newTextItem("accountid", settings.get(0));

        TextItem numberId = ItemFactory.newTextItem("numberid", settings.get(1));

        TextItem accessToken = ItemFactory.newTextItem("accesstoken", settings.get(2));
        accessToken.setWidth(400);

        ToggleItem enabled = ItemFactory.newToggleItem("enabled", "true".equals(settings.get(3)));
        enabled.setEndRow(true);

        ValuesManager vm = new ValuesManager();

        DynamicForm form = new DynamicForm();
        form.setValuesManager(vm);
        form.setTitleOrientation(TitleOrientation.LEFT);
        form.setPadding(5);
        form.setWidth(1);
        form.setNumCols(1);
        form.setTitleOrientation(TitleOrientation.TOP);
        form.setWrapItemTitles(false);

        form.setItems(enabled, accountId, numberId, accessToken);

        IButton save = new IButton(I18N.message("save"));
        save.addClickHandler(click -> onSave(vm));

        body.setMembers(form, save);
    }

    private void onSave(ValuesManager vm) {
        if (Boolean.TRUE.equals(vm.validate())) {
            List<String> settings = new ArrayList<>();
            settings.add(vm.getValueAsString("accountid"));
            settings.add(vm.getValueAsString("numberid"));
            settings.add(vm.getValueAsString("accesstoken"));
            settings.add(vm.getValueAsString("enabled"));

            WhatsappService.Instance.get().saveSettings(settings, new DefaultAsyncCallback<Void>() {

                @Override
                protected void handleSuccess(Void result) {
                    GuiLog.info(I18N.message("settingssaved"), null);
                }
            });
        }
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