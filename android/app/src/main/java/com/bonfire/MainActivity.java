package com.bonfire;

import android.os.Bundle;
import com.facebook.react.ReactActivity;
import com.proyecto26.inappbrowser.RNInAppBrowserModule;

public class MainActivity extends ReactActivity {
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    }

  @Override
  protected void onStart() {
    super.onStart();
    RNInAppBrowserModule.onStart(this);
  }

  /**
   * Returns the name of the main component registered from JavaScript. This is used to schedule
   * rendering of the component.
   */
  @Override
  protected String getMainComponentName() {
    return "bonfire";
  }
}
