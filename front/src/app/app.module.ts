import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { NgbModule } from '@ng-bootstrap/ng-bootstrap';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { ThermalFieldRectangleComponent } from './thermal-field-rectangle/thermal-field-rectangle.component';
import { ThermalFieldTubeComponent } from './thermal-field-tube/thermal-field-tube.component';
import { ElectrostaticFieldSkinComponent } from './electrostatic-field-skin/electrostatic-field-skin.component';
import { ElectrostaticFieldCordComponent } from './electrostatic-field-cord/electrostatic-field-cord.component';
import { ElectromagneticFieldMagnetComponent } from './electromagnetic-field-magnet/electromagnetic-field-magnet.component';
import { ElectromagneticFieldTransformerComponent } from './electromagnetic-field-transformer/electromagnetic-field-transformer.component';
import { ElectrodynamicSystemComponent } from './electrodynamic-system/electrodynamic-system.component';
import { PIXIComponent } from './thermal-field-rectangle/thermal-field-rectangle-pixi.component';

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
    PIXIComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    NgbModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
