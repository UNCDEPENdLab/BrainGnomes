import nibabel as nib
import re
from nilearn.image import resample_img

def parse_entity_from_filename(filename, entity):
    match = re.search(rf"{entity}-([a-zA-Z0-9]+)", filename)
    return match.group(1) if match else None

def normalize_templateflow_value(value):
    if value is None:
        return None
    if isinstance(value, str):
        value = value.strip()
        if value == "" or value.lower() == "none":
            return None
        if re.fullmatch(r"\d+", value):
            return int(value)
    return value

# Helper function to get the voxel resolution from a NIfTI file
def get_voxel_resolution(img):
    zooms = img.header.get_zooms()[:3]
    res = tuple(round(z, 3) for z in zooms)
    return res

# Helper function to parse the image space from the filename
def parse_space_from_filename(filename):
    return parse_entity_from_filename(filename, "space") or "T1w"

# Fetch the template image from TemplateFlow
def fetch_template_image(template, resolution, suffix, desc=None, extension=".nii.gz", cohort=None):
    from templateflow import api

    cohort = normalize_templateflow_value(cohort)
    print(
        f"Fetching from TemplateFlow: template={template}, cohort={cohort}, "
        f"desc={desc}, resolution={resolution}, suffix={suffix}"
    )
    query = dict(
        template=template,
        desc=desc,
        resolution=resolution,
        suffix=suffix,
        extension=extension
    )
    if cohort is not None:
        query["cohort"] = cohort
    return api.get(**query)

def resample_image_to_reference(source_file, reference_file, output, interpolation="nearest"):
    """Resample source voxels onto reference geometry without reading target voxels.

    The reference contributes only its spatial shape, affine and xform codes;
    the output contains the resampled source data.
    """
    source_img = nib.load(source_file)
    reference_img = nib.load(reference_file)

    # resample_to_img validates its target through check_niimg, which can load
    # every BOLD volume. resample_img takes the same spatial geometry directly
    # and leaves the reference's lazy data proxy untouched.
    resampled_img = resample_img(
        img=source_img,
        target_affine=reference_img.affine,
        target_shape=reference_img.shape[:3],
        interpolation=interpolation,
        force_resample=True,
        copy_header=True
    )

    # nilearn copies the target geometry, but nibabel may normalize the xform
    # codes while constructing the resampled image. The output is explicitly
    # on the reference grid, so retain both transforms and codes exactly.
    reference_qform, reference_qform_code = reference_img.get_qform(coded=True)
    reference_sform, reference_sform_code = reference_img.get_sform(coded=True)
    resampled_img.set_qform(reference_qform, code=int(reference_qform_code))
    resampled_img.set_sform(reference_sform, code=int(reference_sform_code))

    nib.save(resampled_img, output)
    return output

def resample_template_to_bold(in_file, output, template_resolution=1, template_space=None,
    template_cohort=None, suffix="mask", desc="brain", extension=".nii.gz", interpolation="nearest"):
    
    # detect the space from the filename if not provided
    if template_space is None:
        template_space = parse_space_from_filename(in_file)
    if template_cohort is None:
        template_cohort = parse_entity_from_filename(in_file, "cohort")
    template_cohort = normalize_templateflow_value(template_cohort)
    
    bold_img = nib.load(in_file)
    resolution = get_voxel_resolution(bold_img)
    print(f"Detected space: {template_space}, cohort: {template_cohort}, resolution: {resolution}")

    # grab the template image from templateflow
    image_path = fetch_template_image(
        template=template_space,
        resolution=template_resolution,
        cohort=template_cohort,
        suffix=suffix,
        desc=desc,
        extension=extension
    )

    if isinstance(image_path, (list, tuple)):
        if len(image_path) == 1:
            image_path = image_path[0]
        else:
            raise ValueError(
                f"TemplateFlow returned multiple files for template={template_space}, "
                f"cohort={template_cohort}, desc={desc}, resolution={template_resolution}"
            )

    return resample_image_to_reference(
        source_file=image_path,
        reference_file=in_file,
        output=output,
        interpolation=interpolation
    )
