import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ElectrodynamicSystemComponent } from './electrodynamic-system/electrodynamic-system.component';
import { ElectromagneticFieldMagnetComponent } from './electromagnetic-field-magnet/electromagnetic-field-magnet.component';
import { ElectromagneticFieldTransformerComponent } from './electromagnetic-field-transformer/electromagnetic-field-transformer.component';
import { ElectrostaticFieldCordComponent } from './electrostatic-field-cord/electrostatic-field-cord.component';
import { ElectrostaticFieldSkinComponent } from './electrostatic-field-skin/electrostatic-field-skin.component';
import { ThermalFieldTubeComponent } from './thermal-field-tube/thermal-field-tube.component';
import { ThermalFieldRectangleComponent } from './thermal_field_rectangle/thermal_field_rectangle.component';

const routes: Routes = [
  { path: '', redirectTo: '/thermal-field-rectangle', pathMatch: 'full' },
  { path: 'thermal-field-rectangle', component: ThermalFieldRectangleComponent },
  { path: 'thermal-field-tube', component: ThermalFieldTubeComponent },
  { path: 'electrostatic-field-skin', component: ElectrostaticFieldSkinComponent },
  { path: 'electrostatic-field-cord', component: ElectrostaticFieldCordComponent },
  { path: 'electromagnetic-field-magnet', component: ElectromagneticFieldMagnetComponent },
  { path: 'electromagnetic-field-transformer', component: ElectromagneticFieldTransformerComponent },
  { path: 'electrodynamic-system', component: ElectrodynamicSystemComponent }

];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }