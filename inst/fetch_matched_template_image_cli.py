import nibabel as nib
import os
import re
import argparse
from templateflow import api
from nilearn.image import resample_to_img
import subprocess

# The header from antsApplyTransforms is not always correct, so we need to fix it
# by copying the header from the reference image (fMRIPrep output)
def fix_header(reference_path, ants_output_path):
    ref_img = nib.load(reference_path)
    warped_img = nib.load(ants_output_path)

    fixed_img = nib.Nifti1Image(warped_img.get_fdata(), affine=ref_img.affine, header=ref_img.header)
    nib.save(fixed_img, ants_output_path)
    print(f"Header fixed for: {ants_output_path}")

# Helper function to get the voxel resolution from a NIfTI file
def get_voxel_resolution(nifti_file):
    img = nib.load(nifti_file)
    zooms = img.header.get_zooms()[:3]
    res = tuple(round(z, 3) for z in zooms)
    return res

# Helper function to parse the image space from the filename
def parse_space_from_filename(filename):
    match = re.search(r"space-([a-zA-Z0-9]+)", filename)
    return match.group(1) if match else "T1w"

# Fetch the template image from TemplateFlow
def fetch_template_image(template, resolution, suffix, desc=None, extension=".nii.gz"):
    print(f"Fetching from TemplateFlow: template={template}, desc={desc}, resolution={resolution}, suffix={suffix}")
    return api.get(
        template=template,
        desc=desc,
        resolution=resolution,
        suffix=suffix,
        extension=extension
    )

# antsApplyTransforms seems not to work as expected -- header isn't right and the mask image is all zeros?
def apply_transform(input_image, reference, output_path, transform=None, interpolation="NearestNeighbor"):
    cmd = [
        "antsApplyTransforms",
        "-d", "3",
        "-i", str(input_image),
        "-r", str(reference),
        "-n", interpolation,
        "-o", str(output_path)
    ]

    if transform:
        cmd.extend(["-t", f"[{transform},1]"])  # inverse transform
    else:
        cmd.extend(["-t", "identity"])

    print(f"Running: {' '.join(cmd)}")
    subprocess.run(cmd, check=True)
    return output_path

def is_native_space(space):
    return space.lower() in ["t1w", "t2w", "anat"]

def main():
    parser = argparse.ArgumentParser(description="Fetch and resample a TemplateFlow image to match a given fMRIPrep image.")
    parser.add_argument("fmriprep_file", help="Path to fMRIPrep output image")
    parser.add_argument("--transform", help="Path to inverse transform (template-to-native); required for native space input.")
    parser.add_argument("--output", default="template_image_resampled.nii.gz", help="Path to output file")
    parser.add_argument("--template-resolution", type=int, default=1,
                        help="TemplateFlow resolution index (e.g., 1 = 1mm)")
    parser.add_argument("--template-space", default="MNI152NLin2009cAsym",
                        help="Template name (used if fmriprep file is in native space)")
    parser.add_argument("--suffix", default="mask", help="Suffix for template image (e.g., 'mask', 'T1w', 'dseg')")
    parser.add_argument("--desc", default="brain", help="Descriptor for template image (e.g., 'brain', 'labels', or leave blank)")
    parser.add_argument("--extension", default=".nii.gz", help="Template file extension (usually .nii.gz)")
    parser.add_argument("--interpolation", default="nearest",
                        choices=["nearest", "continuous", "linear"],
                        help="Nilearn interpolation method: nearest, continuous, linear")

    args = parser.parse_args()

    space = parse_space_from_filename(args.fmriprep_file)
    resolution = get_voxel_resolution(args.fmriprep_file)
    print(f"Detected space: {space}, resolution: {resolution}")

    if is_native_space(space):
        if not args.transform:
            raise ValueError("Native space detected. You must provide --transform to map template to native.")
        
        output_space = args.template_space
    else:
        output_space = space

    # fetch template image
    image_path = fetch_template_image(
        template=output_space,
        resolution=args.template_resolution,
        suffix=args.suffix,
        desc=args.desc,
        extension=args.extension
    )

    # transform
    # apply_transform(
    #     input_image=image_path,
    #     reference=args.fmriprep_file,
    #     output_path=args.output,
    #     transform=args.transform,
    #     interpolation=args.interpolation
    # )
    # fix_header(args.fmriprep_file, args.output)

    template_img = nib.load(image_path)
    bold_img = nib.load(args.fmriprep_file)
    resampled_mask = resample_to_img(template_img, bold_img, interpolation=args.interpolation)

    nib.save(resampled_mask, args.output)

    #     print("Applying inverse transform to bring template into native space...")
        
    #     print(f"Saved native-space template image to: {args.output}")
    # else:
    #     print(f"Image is already in template space ({space}); resampling to match target image.")
    #     image_path = fetch_template_image(
    #         template=space,
    #         resolution=args.template_resolution,
    #         suffix=args.suffix,
    #         desc=args.desc,
    #         extension=args.extension
    #     )
    #     apply_transform(
    #         input_image=image_path,
    #         reference=args.fmriprep_file,
    #         output_path=args.output,
    #         transform=None,
    #         interpolation=args.interpolation
    #     )
    #     print(f"Saved resampled template-space image to: {args.output}")

if __name__ == "__main__":
    main()