import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { HttpClientModule } from '@angular/common/http';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { ThermalFieldRectangleComponent } from './thermal-field-rectangle/thermal-field-rectangle.component';
import { ThermalFieldTubeComponent } from './thermal-field-tube/thermal-field-tube.component';
import { ElectrostaticFieldSkinComponent } from './electrostatic-field-skin/electrostatic-field-skin.component';
import { ElectrostaticFieldCordComponent } from './electrostatic-field-cord/electrostatic-field-cord.component';
import { ElectromagneticFieldMagnetComponent } from './electromagnetic-field-magnet/electromagnetic-field-magnet.component';
import { ElectromagneticFieldTransformerComponent } from './electromagnetic-field-transformer/electromagnetic-field-transformer.component';
import { ElectrodynamicSystemComponent } from './electrodynamic-system/electrodynamic-system.component';
import { ThermalFieldRectanglePixiComponent } from './thermal-field-rectangle/thermal-field-rectangle-pixi.component';
import { ThermalFieldTubePixiComponent } from './thermal-field-tube/thermal-field-tube-pixi.component';
import { ElectrostaticFieldSkinPixiComponent } from './electrostatic-field-skin/electrostatic-field-skin-pixi.component';
import { ElectrostaticFieldCordPixiComponent } from './electrostatic-field-cord/electrostatic-field-cord-pixi.component';
import { ElectromagneticFieldMagnetPixiComponent } from './electromagnetic-field-magnet/electromagnetic-field-magnet-pixi.component';
import { ElectromagneticFieldTransformerPixiComponent } from './electromagnetic-field-transformer/electromagnetic-field-transformer-pixi.component';
import { ElectrodynamicSystemPixiComponent } from './electrodynamic-system/electrodynamic-system-pixi.component';
import { ApiService } from './api.service';

@NgModule({
  declarations: [
    AppComponent,
    ElectrostaticFieldSkinComponent,
    ElectrostaticFieldCordComponent,
    ElectromagneticFieldMagnetComponent,
    ElectromagneticFieldTransformerComponent,
    ElectrodynamicSystemComponent,
    ThermalFieldRectangleComponent,
    ThermalFieldTubeComponent,
    ThermalFieldRectanglePixiComponent,
    ThermalFieldTubePixiComponent,
    ElectrostaticFieldSkinPixiComponent,
    ElectrostaticFieldCordPixiComponent,
    ElectromagneticFieldMagnetPixiComponent,
    ElectromagneticFieldTransformerPixiComponent,
    ElectrodynamicSystemPixiComponent,
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    NgbModule,
    HttpClientModule,
  ],
  providers: [
    ApiService,
  ],
  bootstrap: [AppComponent]
})
export class AppModule { }
