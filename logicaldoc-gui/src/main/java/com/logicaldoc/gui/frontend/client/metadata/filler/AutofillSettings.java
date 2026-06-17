package com.logicaldoc.gui.frontend.client.metadata.filler;

import java.util.Arrays;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;

/**
 * This popup window is used to handle the settings of Autofill
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3
 */
public class AutofillSettings extends Window {

    private ValuesManager vm;

    public AutofillSettings() {
        setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
        setTitle(I18N.message("autofill"));
        setCanDragResize(true);
        setIsModal(true);
        setShowModalMask(true);
        centerInPage();
        setAutoSize(true);

        // Clean the upload folder if the window is closed
        addCloseClickHandler(
                event -> DocumentService.Instance.get().cleanUploadedFileFolder(new DefaultAsyncCallback<>() {
                    @Override
                    public void handleSuccess(Void result) {
                        destroy();
                    }
                }));
    }

    @Override
    protected void onDraw() {
        DynamicForm form = new DynamicForm();
        form.setWidth100();
        form.setAlign(Alignment.LEFT);
        form.setColWidths("1px, 100%");
        vm = new ValuesManager();
        form.setValuesManager(vm);

        SpinnerItem batch = ItemFactory.newSpinnerItem("batch", Session.get().getConfigAsInt("autofill.batch"));
        batch.setStep(50);
        batch.setMin(1);

        ButtonItem save = new ButtonItem("save", I18N.message("save"));
        save.addClickHandler(click -> onSave());

        form.setItems(batch, save);

        addItem(form);
    }

    public void onSave() {
        String batch = vm.getValueAsString("batch");
        Session.get().setConfig("autofill.batch", batch);
        SettingService.Instance.get().saveSettings(Arrays.asList(new GUIParameter("autofill.batch", batch)),
                new DefaultAsyncCallback<>() {
                    @Override
                    public void handleSuccess(Void ret) {
                        destroy();
                    }
                });
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